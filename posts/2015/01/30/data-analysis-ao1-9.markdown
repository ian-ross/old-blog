---
author: Ian
title: Non-diffusive atmospheric flow #9: speeding up KDE
tags: data-analysis,haskell
published: 2015-01-30 15:21:33
---

The Haskell kernel density estimation code in
[the last article][blog8] does work, but it's distressingly slow.
Timing with the Unix `time` command (not all that accurate, but it
gives a good idea of orders of magnitude) reveals that this program
takes about 6.3 seconds to run.  For a one-off, that's not too bad,
but in the next article, we're going to want to run this type of KDE
calculation thousands of times, in order to generate empirical
distributions of null hypothesis PDF values for significance testing.
So we need something faster.

<!--MORE-->

It's quite possible that the Haskell code here could be made quite a
bit faster.  I didn't spend a lot of time thinking about optimising
it.  I originally tried a slightly different approach using a mutable
matrix to accumulate the kernel values across the data points, but
this turned out to be slower than very simple code shown in the
previous article (something like ten times slower!).  It's clear
though that this calculation is very amenable to parallelisation --
the unnormalised PDF value at each point in the $(\theta, \phi)$ grid
can by calculated independently of any other grid point, and the
calculation for each grid point accesses all of the data points in a
very regular way.

So, let's parallelise.  I have an NVIDIA graphics card in my machine
here, so it's very tempting to do something with CUDA.  If I was a
real Haskell programmer, I'd use [Accelerate][accelerate], which is an
embedded DSL for data parallel array processing that has a CUDA
backend.  Unfortunately, a few experiments revealed that it would take
me a while to learn how to use Accelerate, which has some restrictions
on how you can structure algorithms that didn't quite fit with the way
I was trying to do things.  So I gave up on that.

However, I've been writing quite a lot of CUDA C++ recently, so I
decided to use the simple FFI bindings to the CUDA runtime API in the
Haskell `cuda` package, and to write the CUDA code itself in C++.  If
you're at all familiar with CUDA C++, running kernels from Haskell
turns out to be really pretty easy.

I'm not going to get into a long description of CUDA itself here.  You
can read all about it on the [NVIDIA website][nvidia] or there are a
couple of MOOCs that cover parallel programming with CUDA[^1].

Here's the CUDA code for the KDE calculation:

~~~~ {.c}
#include <cuda.h>
#include <cmath>

// Use a regular 2.5 deg. x 2.5 deg. theta/phi grid on unit sphere.
const int Nphi = 2 * 360 / 5, Ntheta = 2 * 180 / 5;

// Integration steps.
const double dphi = 2.0 * M_PI / Nphi, dtheta = M_PI / Ntheta;

// Density kernel bandwidth.
const double bandwidth = M_PI / 6.0;


extern "C"
__global__ void make_pdf_kernel
  (unsigned int D, const double * __restrict__ d_data, double *d_pdf)
{
  unsigned int c = blockIdx.x * blockDim.x + threadIdx.x;
  unsigned int r = blockIdx.y * blockDim.y + threadIdx.y;
  double th = (0.5 + r) * dtheta, ph = c * dphi;
  double gx = sin(th) * cos(ph), gy = sin(th) * sin(ph), gz = cos(th);

  if (r > Ntheta || c > Nphi) return;
  double sum = 0.0;
  for (unsigned int i = 0; i < D; ++i) {
    double dx = d_data[3 * i], dy = d_data[3 * i + 1], dz = d_data[3 * i + 2];
    double u = acos(dx * gx + dy * gy + dz * gz) / bandwidth;
    if (u < 1) sum += 1 - u * u;
  }
  d_pdf[r * Nphi + c] = sum;
}

~~~~

I'll just say a couple of things about it:

 * We launch CUDA threads in two-dimensional blocks set up to cover
   the whole of the $(\theta, \phi)$ grid: the row and column in the
   grid are calculated from the CUDA block and thread indexes in the
   first two lines of the kernel.

 * The main part of the computation is completely straightforward:
   each thread determines the coordinates of the grid point it's
   working on, then loops over all the data points (which are stored
   flattened into the `d_data` vector) accumulating values of the KDE
   Epanechnikov kernel, finally writing the result out to the `d_pdf`
   vector (also a two-dimensional array flattened into a vector in
   row-major order).

 * We do almost nothing to optimise the CUDA code: this is just about
   the simplest CUDA implementation of this algorithm that's possible,
   and it took about five minutes to write.  The only "clever" thing
   is the declaration of the input data point array as `const double *
   __restrict__ d_data`.  This little bit of magic tells the CUDA C++
   compiler that the `d_data` array will not be aliased, contains data
   that will not be modified during the execution of the CUDA kernel,
   and is accessed in a consistent pattern across all threads running
   the kernel.  The upshot of this is that the compiler can generate
   code that causes the GPU to cache this data very aggressively
   (actually, on more recent GPUs, in a very fast L1 cache).  Since
   every thread accesses all of the data points stored in `d_data`,
   this can lead to a large reduction in accesses to global GPU
   memory.  Much optimisation of GPU code comes down to figuring out
   ways to reduce global memory bandwidth, since global memory is much
   slower than the registers and local and shared memory in the
   multiprocessors in the GPU.  The usual approach to this sort of
   thing is to load chunks of data into shared memory ("shared" in
   this context means shared between GPU threads within a single
   thread block), which is faster than global memory.  This requires
   explicitly managing this loading though, which isn't always very
   convenient.  In contrast, telling the CUDA compiler that it can
   cache `d_data` in this way has more or less the same effect, with
   next to no effort.


The CUDA code is compiled to an intermediate format called PTX using
the NVIDIA C++ compiler, `nvcc`:

~~~~
  nvcc -O2 -ptx -arch=compute_50 -code=sm_50 make_pdf.cu
~~~~

The `arch` and `code` options tell the compiler to produce code for
NVIDIA devices with "compute capability" 5.0, which is what the GPU in
my machine has.

Calling the CUDA code from Haskell isn't too hard.  Here, I show just
the parts that are different from the previous Haskell-only approach:

~~~~ {.haskell}
main :: IO ()
main = do
  -- CUDA initialisation.
  CUDA.initialise []
  dev <- CUDA.device 0
  ctx <- CUDA.create dev []
  ptx <- B.readFile "make_pdf.ptx"
  CUDA.JITResult time log mdl <- CUDA.loadDataEx ptx []
  fun <- CUDA.getFun mdl "make_pdf_kernel"

... code omitted ...

  -- Convert projections to 3-D points, flatten to vector and allocate
  -- on device.
  let nData = rows projsin
      dataPts = SV.concat $ map projToPt $ toRows $ cmap realToFrac projsin
  dDataPts <- CUDA.mallocArray $ 3 * nData
  SV.unsafeWith dataPts $ \p -> CUDA.pokeArray (3 * nData) p dDataPts

  -- Calculate kernel values for each grid point/data point
  -- combination and accumulate into grid.
  dPdf <- CUDA.mallocArray $ ntheta * nphi :: IO (CUDA.DevicePtr Double)
  let tx = 32 ; ty = 16
      blockSize = (tx, ty, 1)
      gridSize = ((nphi - 1) `div` tx + 1, (ntheta - 1) `div` ty + 1, 1)
  CUDA.launchKernel fun gridSize blockSize 0 Nothing
    [CUDA.IArg (fromIntegral nData), CUDA.VArg dDataPts, CUDA.VArg dPdf]
  CUDA.sync
  res <- SVM.new (ntheta * nphi)
  SVM.unsafeWith res $ \p -> CUDA.peekArray (ntheta * nphi) dPdf p
  unnormv <- SV.unsafeFreeze res
  let unnorm = reshape nphi unnormv
~~~~

First, we need to initialise the CUDA API and load our compiled
module.  The PTX format is just-in-time compiled to binary code for
the installed GPU during this step, and we output some information
about the compilation process.

Allocating memory on the GPU is done using the `cuda` package's
versions of functions like `mallocArray`, and the `pokeArray` function
is used to transfer data from the host (i.e. the CPU) to the GPU
memory.  The CUDA kernel is then run using the `launchKernel`
function, which takes arguments specifying the layout of threads and
thread blocks to use (the values shown in the code above were the best
I found from doing a couple of quick timing experiments), as well as
the parameters to pass to the kernel function.  Once the kernel
invocation is finished, the results can be retrieved using the
`peekArray` function.

This is all obviously a little bit grungy and it would definitely be
nicer from an aesthetic point of view to use something like
Accelerate, but if you already know CUDA C++ and are familiar with the
CUDA programming model, it's really not that hard to do this sort of
mixed language CPU/GPU programming.

And is it fast?  Oh yeah.  Again, using the very unsophisticated
approach of measuring elapsed time using the Unix `time` utility, we
find that the CUDA version of the KDE code runs in about 0.4 seconds
on my machine.  That's more than ten times faster than the Haskell
only version, on a not very beefy GPU.  And as I've said, I've not put
any real effort into optimising the CUDA code.  I'm sure it could be
made to go faster.  But for our purposes, this is good enough.  In the
next article, we'll want to run this code 10,000 times (you'll see
why), which will take a little over an hour with the CUDA code, rather
than nearly 17 hours with the Haskell-only code.  For writing library
code, or for more performance-critical applications, you would
obviously put a lot more effort into optimisation (think about all the
FFT stuff from earlier articles!), but for this kind of one-off data
analysis task, there's no benefit to spending more time.  It's easy to
set off an analysis job, go for lunch and have the results ready when
you come back.

[^1]: The [Udacity course](https://www.udacity.com/course/cs344) is
      pretty good, as is the Coursera
      [Heterogeneous Parallel Programming](https://www.coursera.org/course/hetero)
      course.

[gist]: https://gist.github.com/ian-ross/6284e75dab0a9b28bd6d
[blog8]: /blog/posts/2015/01/27/data-analysis-ao1-8/index.html
[accelerate]: http://hackage.haskell.org/package/accelerate
[nvidia]: https://developer.nvidia.com/cuda-zone