---
author: Ian
title: Haskell data analysis: Reading NetCDF files
tags: haskell, data-analysis
published: 2014-07-16 21:09:57
---

I never really intended the FFT stuff to go on for as long as it did,
since that sort of thing wasn't really what I was planning as the
focus for this
[Data Analysis in Haskell](/posts/2013/11/05/data-analysis-intro.html)
series.  The FFT was intended primarily as a "warm-up" exercise.
After fourteen blog articles and about 10,000 words, everyone ought to
be sufficiently warmed up now...

Instead of trying to lay out any kind of fundamental principles for
data analysis before we get going, I'm just going to dive into a real
example.  I'll talk about generalities as we go along when we have
some context in which to place them.

All of the analysis described in this next series of articles closely
follows that in the paper:
[D. T. Crommelin (2004). Observed nondiffusive dynamics in large-scale atmospheric flow. *J. Atmos. Sci.* **61**(19), 2384--2396][crommelin].
We're going to replicate most of the data analysis and visualisation
from this paper, maybe adding a few interesting extras towards the
end.

It's going to take a couple of articles to lay out some of the
background to this problem, but I want to start here with something
very practical and not specific to this particular problem.  We're
going to look at how to gain access to meteorological and climate data
stored in the NetCDF file format from Haskell.  This will be useful
not only for the low-frequency atmospheric variability problem we're
going to look at, but for other things in the future too.

<!--MORE-->

### The NetCDF file format

The [NetCDF][netcdf] file format is a "self-describing" binary format
that's used a lot for storing atmospheric and oceanographic data.
It's "self-describing" in the sense that the file format contains
metadata describing the spatial and temporal dimensions of variables,
as well as optional information about units and a bunch of other
stuff.  It's a slightly intimidating format to deal with at first, but
we'll only need to know how a subset of it works.  (And it's much
easier to deal with than HDF5, which we'll probably get around to when
we look at some remote sensing data at some point.)

So, here's the 30-second introduction to NetCDF.  A NetCDF file
contains *dimensions*, *variables* and *attributes*.  A NetCDF
*dimension* just has a name and a size.  One dimension can be
specified as an "unlimited" or record dimension, which is usually used
for time series, and just means that you can tack more records on the
end of the file.  A NetCDF *variable* has a name, a type, a list of
dimensions, some attributes and some data.  As well as *attributes*
attached to variables, a NetCDF file can also have some file-level
global attributes.  A NetCDF *attribute* has a name, a type and a
value.  And that's basically it (for NetCDF-3, at least; NetCDF-4 is a
different beast, but I've never seen a NetCDF-4 file in the wild, so I
don't worry about it too much).


### An example NetCDF file

That's very abstract, so let's look at a real example.  The listing
below shows the output from the `ncdump` tool for one of the data
files we're going to be using, which stores a variable called
geopotential height (I'll explain exactly what this is in a later
article -- for the moment, it's enough to know that it's related to
atmospheric pressure).  The `ncdump` tool is useful for getting a
quick look at what's in a NetCDF file -- it shows all the dimension
and variable definitions, all attributes and also dumps the entire
data contents of the file as ASCII (which you usually want to chop
off...).

```
netcdf z500-1 {
dimensions:
	longitude = 144 ;
	latitude = 73 ;
	time = 7670 ;
variables:
	float longitude(longitude) ;
		longitude:units = "degrees_east" ;
		longitude:long_name = "longitude" ;
	float latitude(latitude) ;
		latitude:units = "degrees_north" ;
		latitude:long_name = "latitude" ;
	int time(time) ;
		time:units = "hours since 1900-01-01 00:00:0.0" ;
		time:long_name = "time" ;
	short z500(time, latitude, longitude) ;
		z500:scale_factor = 0.251043963537454 ;
		z500:add_offset = 50893.8041655182 ;
		z500:_FillValue = -32767s ;
		z500:missing_value = -32767s ;
		z500:units = "m**2 s**-2" ;
		z500:long_name = "Geopotential" ;
		z500:standard_name = "geopotential" ;

// global attributes:
		:Conventions = "CF-1.0" ;
		:history = "Sun Feb  9 18:46:25 2014: ncrename -v z,z500 z500-1.nc\n",
			"2014-01-29 21:04:31 GMT by grib_to_netcdf-1.12.0: grib_to_netcdf /data/soa/scra
tch/netcdf-web237-20140129210048-3022-3037.target -o /data/soa/scratch/netcdf-web237-20140129210411-3022
-3038.nc" ;
data:

 longitude = 0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30,
    32.5, 35, 37.5, 40, 42.5, 45, 47.5, 50, 52.5, 55, 57.5, 60, 62.5, 65,
    67.5, 70, 72.5, 75, 77.5, 80, 82.5, 85, 87.5, 90, 92.5, 95, 97.5, 100,
    102.5, 105, 107.5, 110, 112.5, 115, 117.5, 120, 122.5, 125, 127.5, 130,
    132.5, 135, 137.5, 140, 142.5, 145, 147.5, 150, 152.5, 155, 157.5, 160,
    162.5, 165, 167.5, 170, 172.5, 175, 177.5, 180, 182.5, 185, 187.5, 190,
    192.5, 195, 197.5, 200, 202.5, 205, 207.5, 210, 212.5, 215, 217.5, 220,
    222.5, 225, 227.5, 230, 232.5, 235, 237.5, 240, 242.5, 245, 247.5, 250,
    252.5, 255, 257.5, 260, 262.5, 265, 267.5, 270, 272.5, 275, 277.5, 280,
    282.5, 285, 287.5, 290, 292.5, 295, 297.5, 300, 302.5, 305, 307.5, 310,
    312.5, 315, 317.5, 320, 322.5, 325, 327.5, 330, 332.5, 335, 337.5, 340,
    342.5, 345, 347.5, 350, 352.5, 355, 357.5 ;

 latitude = 90, 87.5, 85, 82.5, 80, 77.5, 75, 72.5, 70, 67.5, 65, 62.5, 60,
    57.5, 55, 52.5, 50, 47.5, 45, 42.5, 40, 37.5, 35, 32.5, 30, 27.5, 25,
    22.5, 20, 17.5, 15, 12.5, 10, 7.5, 5, 2.5, 0, -2.5, -5, -7.5, -10, -12.5,
    -15, -17.5, -20, -22.5, -25, -27.5, -30, -32.5, -35, -37.5, -40, -42.5,
    -45, -47.5, -50, -52.5, -55, -57.5, -60, -62.5, -65, -67.5, -70, -72.5,
    -75, -77.5, -80, -82.5, -85, -87.5, -90 ;
```

As shown in the first line of the listing, this file is called
`z500-1.nc` (it's contains daily 500 millibar geopotential height
data).  It has dimensions called `longitude`, `latitude` and `time`.
There are variables called `longitude`, `latitude`, `time` and `z500`.
The variables with names that are the same as dimensions are called
*coordinate variables* and are part of a metadata convention that
provides information about the file dimensions.  The NetCDF file
format itself doesn't require that dimensions have any more
information provided for them than their name and size, but for most
applications, it makes sense to give units and values for points along
the dimensions.

If we look at the `longitude` variable, we see that it's of type
`float` and has one dimension, which is the `longitude` dimension --
this is how you tell a coordinate variable from a data variable: it
will have the same name as the dimension it goes with and will be
indexed just by that dimension.  Immediately after the line defining
the `longitude` variable are the attributes for the variable.  Here
they give units and a display name (they can also give information
about the range of values and the orientation of the coordinate axis).
All of these attributes are again defined by a metadata convention,
but they're mostly pretty easy to figure out.  Here, the longitude is
given in degrees east of the prime meridian, and if we look further
down the listing, we can see the data values for the `longitude`
variable, running from zero degrees to 357.5&deg;E.  From all this, we
can infer that the 144 longitude values in the file start at the prime
meridian and increase eastwards.

Similarly, the `latitude` variable is a coodinate variable for the
`latitude` dimension, and specifies the latitude of points on the
globe.  The latitude is measured in degrees north of the equator and
ranges from 90&deg; (the North pole) to -90&deg; (the South pole).
Taking a look at the data values for the `latitude` variable, we can
see that 90 degrees north is in index 0, and the 73 latitude values
decrease with increasing index until we reach the South pole.

The `time` coordinate variable is a little more interesting, mostly
because of its units -- this "hours since YYYY-MM-DD HH:MM:SS"
approach to time units is very common in NetCDF files and it's usually
pretty easy to work with.

Finally, we get on to the data variable, `z500`.  This is defined on a
time/latitude/longitude grid (so, in the data, the longitude is the
fastest changing coordinate).  The variable has one slightly odd
feature: its type.  The types for the coordinate variables were all
`float` or `double`, as you'd expect, but `z500` is declared to be a
`short` integer value.  Why?  Well, NetCDF files are quite often *big*
so it can make sense to use some sort of encoding to reduce file
sizes.  (I worked on a paleoclimate modelling project where each model
simulation resulted in about 200 Gb of data, for a dozen models for
half a dozen different scenarios.  In "Big Data" terms, it's not so
large, but it's still quite a bit of data for people to download from
a public server.)  Here, the real-valued geopotential height is packed
into a short integer.  The true value of the field can be recovered
from the short integer values in the file using the `add_offset` and
`scale_factor` attributes -- here the scale factor is unity, so we
just need to add the `add_offset` to each value in the file to get the
geopotential height.

Last of all we have the global attributes in the file.  The most
interesting of these is the `Conventions` attribute, which specifies
that the file uses the [CF metadata convention][cf].  This is the
convention that defines how coordinate variables are represented, how
data values can be compressed by scaling and offsetting, how units and
axes are represented, and so on.  Given a NetCDF file using the CF
convention (or another related convention called the COARDS metadata
convention), it's pretty straightforward to figure out what's going
on.


### Reading NetCDF files in Haskell

So, how do we read NetCDF files into a Haskell program to work on
them?  I've seen a few Haskell FFI bindings to parts of the main
NetCDF C library, but none of those really seemed satisfactory for
day-to-day use, so I've written a simple library called
[hnetcdf][hackage] that includes both a low-level wrapping of the C
library and a more idiomatic Haskell interface (which is what we'll be
using).

In particular, because NetCDF data is usually grid-based, `hnetcdf`
supports reading data values into a number of different kinds of
Haskell arrays (storable `Vector`s, Repa arrays and `hmatrix` arrays).
For this analysis, we're going to use `hmatrix` vectors and matrices,
since they provide a nice "Matlab in Haskell" interface for doing the
sort of linear algebra we'll need.

In this section, we'll look at some simple code for accessing the
NetCDF file whose contents we looked at above which will serve as a
basis for the more complicated things we'll do later.  (The
geopotential height data we're using here is from the
[ERA-Interim][era] reanalysis project -- again, I'll explain what
"reanalysis" means in a later article.  For the moment, think of it as
a "best guess" view of the state of the atmosphere at different
moments in time.)  We'll open the NetCDF file, show how to access the
file metadata and how to read data values from coordinate and data
variables.

We need a few imports first, along with a couple of useful type
synonyms for return values from `hnetcdf` functions:

``` haskell
import Prelude hiding (length, sum)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Foreign.C
import Foreign.Storable
import Numeric.Container
import Data.NetCDF
import Data.NetCDF.HMatrix

type VRet a = IO (Either NcError (HVector a))
type MRet a = IO (Either NcError (HRowMajorMatrix a))
```

As well as a few utility imports and the `Numeric.Container` module
from the `hmatrix` library, we import `Data.NetCDF` and
`Data.NetCDF.HMatrix` -- the first of these is the general `hnetcdf`
API and the second is the module that allows us to use `hnetcdf` with
`hmatrix`.  Most of the functions in `hnetcdf` handle errors by
returning an `Either` of `NcError` and a "useful" return type.  The
`VRet` and `MRet` type synonyms represent return values for vectors
and matrices respectively.  When using `hnetcdf`, it's often necessary
to supply type annotations to control the conversion from NetCDF
values to Haskell values, and these type synonyms come in handy for
doing this.

#### Reading NetCDF metadata

Examining NetCDF metadata is simple:

``` haskell
Right nc <- openFile "/big/data/reanalysis/ERA-Interim/z500-1.nc"
putStrLn $ "Name: " ++ ncName nc
putStrLn $ "Dims: " ++ show (M.keys $ ncDims nc)
putStr $ unlines $ map (\(n, s) -> "  " ++ n ++ ": " ++ s) $
  M.toList $ flip M.map (ncDims nc) $
  \d -> show (ncDimLength d) ++ if ncDimUnlimited d then " (UNLIM)" else ""
putStrLn $ "Vars: " ++ show (M.keys $ ncVars nc)
putStrLn $ "Global attributes: " ++ show (M.keys $ ncAttrs nc)

let Just ntime = ncDimLength <$> ncDim nc "time"
    Just nlat = ncDimLength <$> ncDim nc "latitude"
    Just nlon = ncDimLength <$> ncDim nc "longitude"
```

We open a file using `hnetcdf`'s `openFile` function (here assuming
that there are no errors), getting a value of type `NcInfo` (defined
in `Data.NetCDF.Metadata` in `hnetcdf`).  This is a value representing
all of the metadata in the NetCDF file: dimension, variable and
attribute definitions all bundled up together into a single value from
which we can access different metadata elements.  We can access maps
from names to dimension, variable and global attribute definitions and
can then extract individual dimensions and variables to find
information about them.  The code in the listing above produces this
output for the ERA-Interim $Z_{500}$ NetCDF file used here:

```
Name: /big/data/reanalysis/ERA-Interim/z500-1.nc
Dims: ["latitude","longitude","time"]
  latitude: 73
  longitude: 144
  time: 7670
Vars: ["latitude","longitude","time","z500"]
Global attributes: ["Conventions","history"]
```

#### Accessing coordinate values

Reading values from a NetCDF file requires a little bit of care to
ensure that NetCDF types are mapped correctly to Haskell types:

``` haskell
let (Just lonvar) = ncVar nc "longitude"
Right (HVector lon) <- get nc lonvar :: VRet CFloat
let mlon = mean lon
putStrLn $ "longitude: " ++ show lon ++ " -> " ++ show mlon
Right (HVector lon2) <- getS nc lonvar [0] [72] [2] :: VRet CFloat
let mlon2 = mean lon2
putStrLn $ "longitude (every 2): " ++ show lon2 ++ " -> " ++ show mlon2
```

This shows how to read values from one-dimensional coordinate
variables, both reading the whole variable, using `hnetcdf`'s `get`
function, and reading a strided slice of the data using the `getS`
function.  In both cases, it's necessary to specify the return type of
`get` or `getS` explicitly -- here this is done using the convenience
type synonym `VRet` defined earlier.  This code fragment produces this
output:

```
longitude: fromList [0.0,2.5,5.0,7.5,10.0,12.5,15.0,17.5,20.0,22.5,25.0,
  27.5,30.0,32.5,35.0,37.5,40.0,42.5,45.0,47.5,50.0,52.5,55.0,57.5,60.0,
  62.5,65.0,67.5,70.0,72.5,75.0,77.5,80.0,82.5,85.0,87.5,90.0,92.5,95.0,
  97.5,100.0,102.5,105.0,107.5,110.0,112.5,115.0,117.5,120.0,122.5,125.0,
  127.5,130.0,132.5,135.0,137.5,140.0,142.5,145.0,147.5,150.0,152.5,155.0,
  157.5,160.0,162.5,165.0,167.5,170.0,172.5,175.0,177.5,180.0,182.5,185.0,
  187.5,190.0,192.5,195.0,197.5,200.0,202.5,205.0,207.5,210.0,212.5,215.0,
  217.5,220.0,222.5,225.0,227.5,230.0,232.5,235.0,237.5,240.0,242.5,245.0,
  247.5,250.0,252.5,255.0,257.5,260.0,262.5,265.0,267.5,270.0,272.5,275.0,
  277.5,280.0,282.5,285.0,287.5,290.0,292.5,295.0,297.5,300.0,302.5,305.0,
  307.5,310.0,312.5,315.0,317.5,320.0,322.5,325.0,327.5,330.0,332.5,335.0,
  337.5,340.0,342.5,345.0,347.5,350.0,352.5,355.0,357.5] -> 178.75

longitude (every 2): fromList [0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,
  45.0,50.0,55.0,60.0,65.0,70.0,75.0,80.0,85.0,90.0,95.0,100.0,105.0,110.0,
  115.0,120.0,125.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,165.0,170.0,
  175.0,180.0,185.0,190.0,195.0,200.0,205.0,210.0,215.0,220.0,225.0,230.0,
  235.0,240.0,245.0,250.0,255.0,260.0,265.0,270.0,275.0,280.0,285.0,290.0,
  295.0,300.0,305.0,310.0,315.0,320.0,325.0,330.0,335.0,340.0,345.0,350.0,
  355.0] -> 177.5
```

The `mean` function used in above is defined as:

``` haskell
mean :: (Storable a, Fractional a) => Vector a -> a
mean xs = (foldVector (+) 0 xs) / fromIntegral (dim xs)
```

It requires a `Storable` type class constraint, and makes use of
`hmatrix`'s `foldVector` function.

#### Accessing data values

Finally, we get round to reading the data that we're interested in (of
course, reading the metadata is a necessary prerequisite for this:
this kind of geospatial data doesn't mean much unless you can locate
it in space and time, for which you need coordinate variables and
their associated metadata).

The next listing shows how we read the $Z_{500}$ data into a row-major
`hmatrix` matrix:

``` haskell
let (Just zvar) = ncVar nc "z500"
putStrLn $ "z500 dims: " ++ show (map ncDimName $ ncVarDims zvar)
Right slice1tmp <- getA nc zvar [0, 0, 0] [1, nlat, nlon] :: MRet CShort
let (HRowMajorMatrix slice1tmp2) =
      coardsScale zvar slice1tmp :: HRowMajorMatrix CDouble
    slice1 = cmap ((/ 9.8) . realToFrac) slice1tmp2 :: Matrix Double
putStrLn $ "size slice1 = " ++
  show (rows slice1) ++ " x " ++ show (cols slice1)
putStrLn $ "lon(i=25) = " ++ show (lon @> (25 - 1))
putStrLn $ "lat(j=40) = " ++ show (lat @> (nlat - 40))
let v @!! (i, j) = v @@> (nlat - i, j - 1)
putStrLn $ "slice1(i=25,j=40) = " ++ show (slice1 @!! (25, 40))
```

There are a number of things to note here.  First, we use the `getA`
function, which allows us to specify starting indexes and counts for
each dimension in the variable we're reading.  Here we read all
latitude and longitude points for a single vertical level in the
atmosphere (which is the only one there is in this file).  Second, the
values stored in this file are *geopotential* values, not geopotential
height (so their units are m s<sup>-2</sup> instead of metres, which
we can convert to geopotential height by dividing by the acceleration
due to gravity (about 9.8 m s<sup>-2</sup>).  Third, the geopotential
values are stored in a compressed form as short integers according to
the COARDS metadata convention.  This means that if we want to work
with floating point values (which we almost always do), we need to
convert using the `hnetcdf` `coardsScale` function, which reads the
relevant scaling and offset attributes from the NetCDF variable and
uses them to convert from the stored data values to some fractional
numeric type (in this case `CDouble` -- the destination type also
needs to be an instance of `hnetcdf`'s `NcStorable` class).

Once we have the input data converted to a normal `hmatrix` `Matrix`
value, we can manipulate it like any other data value.  In particular,
here we extract the geopotential height at given latitude and
longitude coordinates (the `@!!` operator defined here is just a
custom indexing operator to deal with the fact that the latitude
values are stored in north-to-south order).

The most laborious part of all this is managing the correspondence
between coordinate values and indexes, and managing the conversions
between the C types used to represent values stored in NetCDF files
(`CDouble`, `CShort`, etc.) and the native Haskell types that we'd
like to use for our data manipulation activities.  To be fair, the
first of these problems is a problem for any user of NetCDF files, and
Haskell's data abstraction capabilities at least make dealing with
metadata values less onerous than in C or C++.  The second issue is a
little more annoying, but it does ensure that we maintain a good
*cordon sanitaire* between external representations of data values and
the internal representations that we use.


### What's next

We're going to have to spend a couple of articles covering some
background to the atmospheric variability problem we're going to look
at, just to place some of this stuff in context: we need to look a
little at just what this study is trying to address, we need to
understand some basic facts about atmospheric dynamics and the data
we're going to be using, and we need to take a look at the gross
dynamics of the atmosphere as they appear in these data, just so that
we have some sort of idea what we're looking at later on.  That will
probably take two or three articles, but then we can start with some
real data analysis.


[crommelin]: http://journals.ametsoc.org/doi/full/10.1175/1520-0469%282004%29061%3C2384%3AONDILA%3E2.0.CO%3B2
[netcdf]: http://www.unidata.ucar.edu/software/netcdf/
[cf]: http://cfconventions.org/
[hackage]: http://hackage.haskell.org/package/hnetcdf-0.2.0.0
[era]: http://data-portal.ecmwf.int/data/d/interim_daily
