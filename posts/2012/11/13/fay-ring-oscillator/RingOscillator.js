/** @constructor
*/
var RingOscillator = function(){
var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
    while (thunkish instanceof $) {
        thunkish = thunkish.force(nocache);
    }
    return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
    var f = arguments[0];
    for (var i = 1, len = arguments.length; i < len; i++) {
        f = (f instanceof $? _(f) : f)(arguments[i]);
    }
    return f;
}

// Thunk object.
function $(value){
    this.forced = false;
    this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
    this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
    return function(b){
      return Fay$$bind(a)(function(_){
        return b;
      });
    };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new $(function(){
      var monad = _(m,true);
      if(monad.cont) {
        return _(monad.cont(f));
      }
      else {
        return f(monad.value);
      }
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
    return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
    var base = type[0];
    var args = type[1];
    var jsObj;
    switch(base){
    case "action": {
        // A nullary monadic action. Should become a nullary JS function.
        // Fay () -> function(){ return ... }
        jsObj = function(){
            return Fay$$fayToJs(args[0],_(fayObj,true).value);
        };
        break;
    }
    case "function": {
        // A proper function.
        jsObj = function(){
            var fayFunc = fayObj;
            var return_type = args[args.length-1];
            var len = args.length;
            // If some arguments.
            if (len > 1) {
                // Apply to all the arguments.
                fayFunc = _(fayFunc,true);
                // TODO: Perhaps we should throw an error when JS
                // passes more arguments than Haskell accepts.
                for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
                    // Unserialize the JS values to Fay for the Fay callback.
                    fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
                }
                // Finally, serialize the Fay return value back to JS.
                var return_base = return_type[0];
                var return_args = return_type[1];
                // If it's a monadic return value, get the value instead.
                if(return_base == "action") {
                    return Fay$$fayToJs(return_args[0],fayFunc.value);
                }
                // Otherwise just serialize the value direct.
                else {
                    return Fay$$fayToJs(return_type,fayFunc);
                }
            } else {
                throw new Error("Nullary function?");
            }
        };
        break;
    }
    case "string": {
        // Serialize Fay string to JavaScript string.
        var str = "";
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            str += fayObj.car;
            fayObj = _(fayObj.cdr);
        }
        jsObj = str;
        break;
    }
    case "list": case "tuple": {
        // Serialize Fay list or tuple to JavaScript array.
        var arr = [];
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            arr.push(Fay$$fayToJs(args[0],fayObj.car));
            fayObj = _(fayObj.cdr);
        }
        jsObj = arr;
        break;
    }
    case "double": {
        // Serialize double, just force the argument. Doubles are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "int": {
        // Serialize int, just force the argument. Ints are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "bool": {
        // Bools are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "unknown":
    case "user": {
        if(fayObj instanceof $)
            fayObj = _(fayObj);
        jsObj = Fay$$fayToJsUserDefined(type,fayObj);
        break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
    var base = type[0];
    var args = type[1];
    var fayObj;
    switch(base){
    case "action": {
        // Unserialize a "monadic" JavaScript return value into a monadic value.
        fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
        break;
    }
    case "string": {
        // Unserialize a JS string into Fay list (String).
        fayObj = Fay$$list(jsObj);
        break;
    }
    case "list": {
        // Unserialize a JS array into a Fay list ([a]).
        var serializedList = [];
        for (var i = 0, len = jsObj.length; i < len; i++) {
            // Unserialize each JS value into a Fay value, too.
            serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
        }
        // Pop it all in a Fay list.
        fayObj = Fay$$list(serializedList);
        break;
    }
    case "double": {
        // Doubles are unboxed, so there's nothing to do.
        fayObj = jsObj;
        break;
    }
    case "int": {
        // Int are unboxed, so there's no forcing to do.
        // But we can do validation that the int has no decimal places.
        // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
        fayObj = Math.round(jsObj);
        if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
        break;
    }
    case "bool": {
        // Bools are unboxed.
        fayObj = jsObj;
        break;
    }
    case "unknown":
    case "user": {
        if (jsObj && jsObj['instance']) {
            fayObj = Fay$$jsToFayUserDefined(type,jsObj);
        }
        else
            fayObj = jsObj;
        break;
    }
    default: throw new Error("Unhandled JS->Fay translation type: " + base);
    }
    return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
    this.car = car;
    this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
    var out = null;
    for(var i=xs.length-1; i>=0;i--)
        out = new Fay$$Cons(xs[i],out);
    return out;
}

// Built-in list cons.
function Fay$$cons(x){
    return function(y){
        return new Fay$$Cons(x,y);
    };
}

// List index.
function Fay$$index(index){
    return function(list){
        for(var i = 0; i < index; i++) {
            list = _(list).cdr;
        }
        return list.car;
    };
}

// List length.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = _(list).cdr;
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
    return function(y){
        return new $(function(){
            return _(x) * _(y);
        });
    };
}

// Built-in +.
function Fay$$add(x){
    return function(y){
        return new $(function(){
            return _(x) + _(y);
        });
    };
}

// Built-in -.
function Fay$$sub(x){
    return function(y){
        return new $(function(){
            return _(x) - _(y);
        });
    };
}

// Built-in /.
function Fay$$div(x){
    return function(y){
        return new $(function(){
            return _(x) / _(y);
        });
    };
}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
    // Simple case
    lit1 = _(lit1);
    lit2 = _(lit2);
    if (lit1 === lit2) {
        return true;
    }
    // General case
    if (lit1 instanceof Array) {
        if (lit1.length != lit2.length) return false;
        for (var len = lit1.length, i = 0; i < len; i++) {
            if (!Fay$$equal(lit1[i], lit2[i])) return false;
        }
        return true;
    } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
        do {
            if (!Fay$$equal(lit1.car,lit2.car))
                return false;
            lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
            if (lit1 === null || lit2 === null)
                return lit1 === lit2;
        } while (true);
    } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
              lit1.constructor === lit2.constructor) {
      for(var x in lit1) {
        if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
            Fay$$equal(lit1[x],lit2[x])))
          return false;
      }
      return true;
    } else {
      return false;
    }
}

// Built-in ==.
function Fay$$eq(x){
    return function(y){
        return new $(function(){
            return Fay$$equal(x,y);
        });
    };
}

// Built-in /=.
function Fay$$neq(x){
    return function(y){
        return new $(function(){
            return !(Fay$$equal(x,y));
        });
    };
}

// Built-in >.
function Fay$$gt(x){
    return function(y){
        return new $(function(){
            return _(x) > _(y);
        });
    };
}

// Built-in <.
function Fay$$lt(x){
    return function(y){
        return new $(function(){
            return _(x) < _(y);
        });
    };
}

// Built-in >=.
function Fay$$gte(x){
    return function(y){
        return new $(function(){
            return _(x) >= _(y);
        });
    };
}

// Built-in <=.
function Fay$$lte(x){
    return function(y){
        return new $(function(){
            return _(x) <= _(y);
        });
    };
}

// Built-in &&.
function Fay$$and(x){
    return function(y){
        return new $(function(){
            return _(x) && _(y);
        });
    };
}

// Built-in ||.
function Fay$$or(x){
    return function(y){
        return new $(function(){
            return _(x) || _(y);
        });
    };
}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
    this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
    ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
    return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
    return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$Stdlib$show = function($p1){return new $(function(){return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["unknown"],$p1)));});};var Language$Fay$Stdlib$fromInteger = function($p1){return new $(function(){var x = $p1;return x;});};var Language$Fay$Stdlib$fromRational = function($p1){return new $(function(){var x = $p1;return x;});};var Language$Fay$Stdlib$snd = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(1)(_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};var Language$Fay$Stdlib$fst = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(0)(_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};var Language$Fay$Stdlib$find = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(Language$Fay$Stdlib$Just)(x) : _(_(Language$Fay$Stdlib$find)(p))(xs);}if (_($p2) === null) {return Language$Fay$Stdlib$Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};var Language$Fay$Stdlib$any = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? true : _(_(Language$Fay$Stdlib$any)(p))(xs);}if (_($p2) === null) {return false;}throw ["unhandled case in any",[$p1,$p2]];});};};var Language$Fay$Stdlib$filter = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$filter)(p))(xs)) : _(_(Language$Fay$Stdlib$filter)(p))(xs);}if (_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};var Language$Fay$Stdlib$not = function($p1){return new $(function(){var p = $p1;return _(p) ? false : true;});};var Language$Fay$Stdlib$$_null = function($p1){return new $(function(){if (_($p1) === null) {return true;}return false;});};var Language$Fay$Stdlib$map = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(f)(x)))(_(_(Language$Fay$Stdlib$map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};var Language$Fay$Stdlib$nub = function($p1){return new $(function(){var ls = $p1;return _(_(Language$Fay$Stdlib$nub$39$)(ls))(null);});};var Language$Fay$Stdlib$nub$39$ = function($p1){return function($p2){return new $(function(){if (_($p1) === null) {return null;}var ls = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(_(Language$Fay$Stdlib$elem)(x))(ls)) ? _(_(Language$Fay$Stdlib$nub$39$)(xs))(ls) : _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$nub$39$)(xs))(_(_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};var Language$Fay$Stdlib$elem = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return _(Fay$$or)(_(_(_(Fay$$eq)(x))(y)))(_(_(_(Language$Fay$Stdlib$elem)(x))(ys)));}if (_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};var $_Language$Fay$Stdlib$GT = function(){};var Language$Fay$Stdlib$GT = new $(function(){return new $_Language$Fay$Stdlib$GT();});var $_Language$Fay$Stdlib$LT = function(){};var Language$Fay$Stdlib$LT = new $(function(){return new $_Language$Fay$Stdlib$LT();});var $_Language$Fay$Stdlib$EQ = function(){};var Language$Fay$Stdlib$EQ = new $(function(){return new $_Language$Fay$Stdlib$EQ();});var Language$Fay$Stdlib$sort = new $(function(){return _(Language$Fay$Stdlib$sortBy)(Language$Fay$Stdlib$compare);});var Language$Fay$Stdlib$compare = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Fay$$gt)(_(x))(_(y))) ? Language$Fay$Stdlib$GT : _(_(Fay$$lt)(_(x))(_(y))) ? Language$Fay$Stdlib$LT : Language$Fay$Stdlib$EQ;});};};var Language$Fay$Stdlib$sortBy = function($p1){return new $(function(){var cmp = $p1;return _(_(Language$Fay$Stdlib$foldr)(_(Language$Fay$Stdlib$insertBy)(cmp)))(null);});};var Language$Fay$Stdlib$insertBy = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (_($tmp2) instanceof $_Language$Fay$Stdlib$GT) {return _(_(Fay$$cons)(y))(_(_(_(Language$Fay$Stdlib$insertBy)(cmp))(x))(ys$39$));}return _(_(Fay$$cons)(x))(ys);})(_(_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};var Language$Fay$Stdlib$when = function($p1){return function($p2){return new $(function(){var m = $p2;var p = $p1;return _(p) ? _(_(Fay$$then)(m))(_(Fay$$$_return)(Fay$$unit)) : _(Fay$$$_return)(Fay$$unit);});};};var Language$Fay$Stdlib$enumFrom = function($p1){return new $(function(){var i = $p1;return _(_(Fay$$cons)(i))(_(Language$Fay$Stdlib$enumFrom)(_(Fay$$add)(_(i))(1)));});};var Language$Fay$Stdlib$enumFromTo = function($p1){return function($p2){return new $(function(){var n = $p2;var i = $p1;return _(_(_(Fay$$eq)(i))(n)) ? Fay$$list([i]) : _(_(Fay$$cons)(i))(_(_(Language$Fay$Stdlib$enumFromTo)(_(Fay$$add)(_(i))(1)))(n));});};};var Language$Fay$Stdlib$zipWith = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(_(f)(a))(b)))(_(_(_(Language$Fay$Stdlib$zipWith)(f))(as))(bs));}}return null;});};};};var Language$Fay$Stdlib$zip = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b])))(_(_(Language$Fay$Stdlib$zip)(as))(bs));}}return null;});};};var Language$Fay$Stdlib$flip = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var f = $p1;return _(_(f)(y))(x);});};};};var Language$Fay$Stdlib$maybe = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) instanceof $_Language$Fay$Stdlib$Nothing) {var m = $p1;return m;}if (_($p3) instanceof $_Language$Fay$Stdlib$Just) {var x = _($p3).slot1;var f = $p2;return _(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$$46$ = function($p1){return function($p2){return function($p3){return new $(function(){var x = $p3;var g = $p2;var f = $p1;return _(f)(_(g)(x));});};};};var Language$Fay$Stdlib$$43$$43$ = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Language$Fay$Stdlib$conc)(x))(y);});};};var Language$Fay$Stdlib$$36$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(f)(x);});};};var Language$Fay$Stdlib$conc = function($p1){return function($p2){return new $(function(){var ys = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$conc)(xs))(ys));}var ys = $p2;if (_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};var Language$Fay$Stdlib$concat = new $(function(){return _(_(Language$Fay$Stdlib$foldr)(Language$Fay$Stdlib$conc))(null);});var Language$Fay$Stdlib$concatMap = function($p1){return new $(function(){var f = $p1;return _(_(Language$Fay$Stdlib$foldr)(_(_(Language$Fay$Stdlib$$46$)(Language$Fay$Stdlib$$43$$43$))(f)))(null);});};var Language$Fay$Stdlib$foldr = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(f)(x))(_(_(_(Language$Fay$Stdlib$foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$foldl = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(_(Language$Fay$Stdlib$foldl)(f))(_(_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$lookup = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {var _key = $p1;return Language$Fay$Stdlib$Nothing;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),2)) {var x = Fay$$index(0)(_($tmp1.car));var y = Fay$$index(1)(_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return _(_(_(Fay$$eq)(key))(x)) ? _(Language$Fay$Stdlib$Just)(y) : _(_(Language$Fay$Stdlib$lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};var Language$Fay$Stdlib$intersperse = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};var Language$Fay$Stdlib$prependToAll = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(sep))(_(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};var Language$Fay$Stdlib$intercalate = function($p1){return function($p2){return new $(function(){var xss = $p2;var xs = $p1;return _(Language$Fay$Stdlib$concat)(_(_(Language$Fay$Stdlib$intersperse)(xs))(xss));});};};var Language$Fay$Stdlib$forM_ = function($p1){return function($p2){return new $(function(){var m = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$then)(_(m)(x)))(_(_(Language$Fay$Stdlib$forM_)(xs))(m));}if (_($p1) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};var Language$Fay$Stdlib$mapM_ = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return _(_(Fay$$then)(_(m)(x)))(_(_(Language$Fay$Stdlib$mapM_)(m))(xs));}if (_($p2) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};var Language$Fay$Stdlib$$_const = function($p1){return function($p2){return new $(function(){var a = $p1;return a;});};};var Language$Fay$Stdlib$length = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;return _(Fay$$add)(1)(_(_(Language$Fay$Stdlib$length)(xs)));}if (_($p1) === null) {return 0;}throw ["unhandled case in length",[$p1]];});};var Language$Fay$Stdlib$mod = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["double"],$p1) % Fay$$fayToJs(["double"],$p2));});};};var Language$Fay$Stdlib$min = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["double"],Math.min(Fay$$fayToJs(["double"],$p1),Fay$$fayToJs(["double"],$p2)));});};};var Language$Fay$Stdlib$max = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["double"],Math.max(Fay$$fayToJs(["double"],$p1),Fay$$fayToJs(["double"],$p2)));});};};var Language$Fay$Stdlib$fromIntegral = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$p1));});};var Language$Fay$Stdlib$otherwise = true;var Language$Fay$Stdlib$reverse = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Language$Fay$Stdlib$$43$$43$)(_(Language$Fay$Stdlib$reverse)(xs)))(Fay$$list([x]));}if (_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};var Language$Fay$Stdlib$$61$$60$$60$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$bind)(x))(f);});};};var Language$Fay$Stdlib$sequence = function($p1){return new $(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new $(function(){var m$39$ = $p2;var m = $p1;return _(_(Fay$$bind)(m))(function($p1){var x = $p1;return _(_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return _(Fay$$$_return)(_(_(Fay$$cons)(x))(xs));});});});};};return _(_(_(Language$Fay$Stdlib$foldr)(k))(_(Fay$$$_return)(null)))(ms);})();});};var $_Language$Fay$Stdlib$Just = function(slot1){this.slot1 = slot1;};var Language$Fay$Stdlib$Just = function(slot1){return new $(function(){return new $_Language$Fay$Stdlib$Just(slot1);});};var $_Language$Fay$Stdlib$Nothing = function(){};var Language$Fay$Stdlib$Nothing = new $(function(){return new $_Language$Fay$Stdlib$Nothing();});var $_RingOscillator$Params = function(alpha,omega,deven,dodd,beta,sigma,nosc){this.alpha = alpha;this.omega = omega;this.deven = deven;this.dodd = dodd;this.beta = beta;this.sigma = sigma;this.nosc = nosc;};var RingOscillator$Params = function(alpha){return function(omega){return function(deven){return function(dodd){return function(beta){return function(sigma){return function(nosc){return new $(function(){return new $_RingOscillator$Params(alpha,omega,deven,dodd,beta,sigma,nosc);});};};};};};};};var RingOscillator$alpha = function(x){return new $(function(){return _(x).alpha;});};var RingOscillator$omega = function(x){return new $(function(){return _(x).omega;});};var RingOscillator$deven = function(x){return new $(function(){return _(x).deven;});};var RingOscillator$dodd = function(x){return new $(function(){return _(x).dodd;});};var RingOscillator$beta = function(x){return new $(function(){return _(x).beta;});};var RingOscillator$sigma = function(x){return new $(function(){return _(x).sigma;});};var RingOscillator$nosc = function(x){return new $(function(){return _(x).nosc;});};var RingOscillator$alphaUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.alpha = val;return $36$_record_to_update;})();});};};var RingOscillator$omegaUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.omega = val;return $36$_record_to_update;})();});};};var RingOscillator$devenUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.deven = val;return $36$_record_to_update;})();});};};var RingOscillator$doddUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.dodd = val;return $36$_record_to_update;})();});};};var RingOscillator$betaUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.beta = val;return $36$_record_to_update;})();});};};var RingOscillator$sigmaUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.sigma = val;return $36$_record_to_update;})();});};};var RingOscillator$noscUpd = function($p1){return function($p2){return new $(function(){var val = $p2;var p = $p1;return (function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.nosc = val;return $36$_record_to_update;})();});};};var RingOscillator$defaultParams = new $(function(){var Params = new $_RingOscillator$Params();Params.alpha = 1;Params.omega = 1.8;Params.deven = 7.5e-3;Params.dodd = 1.25e-2;Params.beta = 1;Params.sigma = 4;Params.nosc = 5;return Params;});var RingOscillator$initialState = function($p1){return new $(function(){var p = $p1;return _(_(Language$Fay$Stdlib$$43$$43$)(_(_(RingOscillator$replicate)(_(Fay$$add)(_(_(RingOscillator$nosc)(p)))(1)))(1)))(_(_(RingOscillator$replicate)(_(Fay$$add)(_(_(RingOscillator$nosc)(p)))(1)))(0));});};var RingOscillator$ic = new $(function(){return _(_(RingOscillator$centre)(RingOscillator$defaultParams))(_(RingOscillator$initialState)(RingOscillator$defaultParams));});var RingOscillator$rhs = function($p1){return function($p2){return new $(function(){var state = $p2;var p = $p1;return (function(){var n = new $(function(){return _(RingOscillator$nosc)(p);});var ss = new $(function(){return _(_(RingOscillator$splitAt)(_(Fay$$add)(_(n))(1)))(state);});var sss = new $(function(){return _(Language$Fay$Stdlib$fst)(ss);});var sss$39$ = new $(function(){return _(Language$Fay$Stdlib$snd)(ss);});var y = new $(function(){return _(RingOscillator$head)(sss);});var xs = new $(function(){return _(RingOscillator$tail)(sss);});var y$39$ = new $(function(){return _(RingOscillator$head)(sss$39$);});var xs$39$ = new $(function(){return _(RingOscillator$tail)(sss$39$);});var dy = new $(function(){return y$39$;});var dxs = new $(function(){return xs$39$;});var dy$39$ = new $(function(){return _(Fay$$sub)(_(_(Fay$$sub)(0)(_(_(Fay$$mult)(_(_(Fay$$mult)(_(_(RingOscillator$alpha)(p)))(_(_(Fay$$sub)(_(_(_(RingOscillator$$94$)(y))(2)))(1)))))(_(y$39$))))))(_(_(Fay$$mult)(_(_(_(RingOscillator$$94$)(_(RingOscillator$omega)(p)))(2)))(_(y))));});var xdiffs = new $(function(){return _(_(Fay$$cons)(_(Fay$$sub)(_(_(RingOscillator$head)(xs)))(_(_(_(RingOscillator$$33$$33$)(xs))(_(Fay$$sub)(_(n))(1))))))(_(_(_(Language$Fay$Stdlib$zipWith)(Fay$$sub))(_(RingOscillator$tail)(xs)))(xs));});var vprime = function($p1){return new $(function(){var x = $p1;return _(Fay$$add)(_(x))(_(_(_(RingOscillator$$94$)(x))(3)));});};var vp = new $(function(){return _(_(Language$Fay$Stdlib$map)(vprime))(xdiffs);});var vfacs = new $(function(){return _(_(Language$Fay$Stdlib$$43$$43$)(_(_(_(Language$Fay$Stdlib$zipWith)(Fay$$sub))(vp))(_(RingOscillator$tail)(vp))))(Fay$$list([_(Fay$$sub)(_(_(_(RingOscillator$$33$$33$)(vp))(_(Fay$$sub)(_(n))(1))))(_(_(RingOscillator$head)(vp)))]));});var ds = new $(function(){return _(RingOscillator$cycle)(Fay$$list([_(RingOscillator$dodd)(p),_(RingOscillator$deven)(p)]));});var dxstmp = new $(function(){return _(_(_(_(RingOscillator$zipWith3)(function($p1){var d = $p1;return function($p2){var x$39$ = $p2;return function($p3){var vf = $p3;return _(Fay$$sub)(_(_(Fay$$sub)(0)(_(_(Fay$$mult)(_(d))(_(x$39$))))))(_(_(Fay$$mult)(_(_(RingOscillator$beta)(p)))(_(vf))));};};}))(ds))(xs$39$))(vfacs);});var dxs$39$ = new $(function(){return _(_(Fay$$cons)(_(Fay$$add)(_(_(RingOscillator$head)(dxstmp)))(_(_(Fay$$mult)(_(_(RingOscillator$sigma)(p)))(_(y))))))(_(RingOscillator$tail)(dxstmp));});return _(_(Language$Fay$Stdlib$$43$$43$)(Fay$$list([dy])))(_(_(Language$Fay$Stdlib$$43$$43$)(dxs))(_(_(Language$Fay$Stdlib$$43$$43$)(Fay$$list([dy$39$])))(dxs$39$)));})();});};};var RingOscillator$rk4 = function($p1){return function($p2){return function($p3){return new $(function(){var h = $p3;var yn = $p2;var f = $p1;return (function(){var k1 = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$map)(mh)))(_(f)(yn));});var k2 = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$map)(mh)))(_(f)(_(_(_(Language$Fay$Stdlib$zipWith)(Fay$$add))(yn))(_(_(Language$Fay$Stdlib$map)(half))(k1))));});var k3 = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$map)(mh)))(_(f)(_(_(_(Language$Fay$Stdlib$zipWith)(Fay$$add))(yn))(_(_(Language$Fay$Stdlib$map)(half))(k2))));});var k4 = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$map)(mh)))(_(f)(_(_(_(Language$Fay$Stdlib$zipWith)(Fay$$add))(yn))(k3)));});var mh = function($p1){return new $(function(){var x = $p1;return _(Fay$$mult)(_(h))(_(x));});};var half = function($p1){return new $(function(){var x = $p1;return _(Fay$$mult)(0.5)(_(x));});};return _(_(_(_(_(_(RingOscillator$zipWith5)(function($p1){var y = $p1;return function($p2){var a = $p2;return function($p3){var b = $p3;return function($p4){var c = $p4;return function($p5){var d = $p5;return _(Fay$$add)(_(y))(_(_(Fay$$div)(_(_(Fay$$add)(_(_(Fay$$add)(_(_(Fay$$add)(_(a))(_(_(Fay$$mult)(2)(_(b))))))(_(_(Fay$$mult)(2)(_(c))))))(_(d))))(6)));};};};};}))(yn))(k1))(k2))(k3))(k4);})();});};};};var RingOscillator$centre = function($p1){return function($p2){return new $(function(){var s = $p2;var p = $p1;return (function(){var ss = new $(function(){return _(_(RingOscillator$splitAt)(_(Fay$$add)(_(_(RingOscillator$nosc)(p)))(1)))(s);});var sss = new $(function(){return _(Language$Fay$Stdlib$fst)(ss);});var sss$39$ = new $(function(){return _(Language$Fay$Stdlib$snd)(ss);});var y = new $(function(){return _(RingOscillator$head)(sss);});var xs = new $(function(){return _(RingOscillator$tail)(sss);});var y$39$ = new $(function(){return _(RingOscillator$head)(sss$39$);});var xs$39$ = new $(function(){return _(RingOscillator$tail)(sss$39$);});var m = new $(function(){return _(Fay$$div)(_(_(RingOscillator$sum)(xs)))(_(_(Language$Fay$Stdlib$fromIntegral)(_(Language$Fay$Stdlib$length)(xs))));});var xsc = new $(function(){return _(_(Language$Fay$Stdlib$map)(function($p1){var x = $p1;return _(Fay$$sub)(_(x))(_(m));}))(xs);});return _(_(Language$Fay$Stdlib$$43$$43$)(Fay$$list([y])))(_(_(Language$Fay$Stdlib$$43$$43$)(xsc))(_(_(Language$Fay$Stdlib$$43$$43$)(Fay$$list([y$39$])))(xs$39$)));})();});};};var RingOscillator$ww = 400;var RingOscillator$wh = 400;var RingOscillator$gww = 640;var RingOscillator$gwh = 200;var RingOscillator$dt = 2.5e-2;var RingOscillator$framems = 30;var RingOscillator$renderrng = 6.0;var RingOscillator$rdotfac = 3.75e-2;var RingOscillator$rinner = new $(function(){return _(Fay$$div)(_(RingOscillator$wh))(_(_(Fay$$add)(2)(_(_(Fay$$mult)(7)(_(RingOscillator$rdotfac))))));});var RingOscillator$rdot = new $(function(){return _(Fay$$mult)(_(RingOscillator$rdotfac))(_(RingOscillator$rinner));});var RingOscillator$fyoff = new $(function(){return _(Fay$$add)(_(RingOscillator$rinner))(_(_(Fay$$mult)(3)(_(RingOscillator$rdot))));});var RingOscillator$ctr = new $(function(){return Fay$$list([_(Fay$$mult)(0.5)(_(RingOscillator$ww)),_(Fay$$add)(_(_(Fay$$mult)(5)(_(RingOscillator$rdot))))(_(RingOscillator$rinner))]);});var RingOscillator$gwbuf = 20;var RingOscillator$ticklen = 10;var RingOscillator$gwwtime = 10;var RingOscillator$pxpert = new $(function(){return _(Fay$$div)(_(RingOscillator$gww))(_(RingOscillator$gwwtime));});var RingOscillator$pxpersamp = new $(function(){return _(Fay$$mult)(_(RingOscillator$dt))(_(RingOscillator$pxpert));});var RingOscillator$nsamp = new $(function(){return _(RingOscillator$floor)(_(Fay$$div)(_(_(Fay$$sub)(_(RingOscillator$gww))(_(RingOscillator$gwbuf))))(_(RingOscillator$pxpersamp)));});var RingOscillator$main = new $(function(){return _(_(RingOscillator$addWindowEventListener)(Fay$$list("load")))(RingOscillator$run);});var RingOscillator$run = function($p1){return new $(function(){return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("canvas"),Fay$$list("graph")]))))(function($p1){if (Fay$$listLen(_($p1),2)) {var can = Fay$$index(0)(_($p1));var graph = Fay$$index(1)(_($p1));return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("go"),Fay$$list("stop"),Fay$$list("reset")]))))(function($p1){if (Fay$$listLen(_($p1),3)) {var go = Fay$$index(0)(_($p1));var stop = Fay$$index(1)(_($p1));var reset = Fay$$index(2)(_($p1));return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(RingOscillator$getElementById))(Fay$$list([Fay$$list("alpha"),Fay$$list("omega"),Fay$$list("deven"),Fay$$list("dodd"),Fay$$list("beta"),Fay$$list("sigma")]))))(function($p1){var sels = $p1;return _(_(Fay$$bind)(_(RingOscillator$getElementById)(Fay$$list("nosc"))))(function($p1){var noscSel = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(function($p1){var c = $p1;return _(_(RingOscillator$getContext)(c))(Fay$$list("2d"));}))(Fay$$list([can,graph]))))(function($p1){if (Fay$$listLen(_($p1),2)) {var c = Fay$$index(0)(_($p1));var cg = Fay$$index(1)(_($p1));return _(_(Fay$$bind)(_(RingOscillator$newRef)(RingOscillator$defaultParams)))(function($p1){var pref = $p1;return _(_(Fay$$bind)(_(RingOscillator$newRef)(RingOscillator$ic)))(function($p1){var xref = $p1;return _(_(Fay$$bind)(_(RingOscillator$newRef)(Language$Fay$Stdlib$Nothing)))(function($p1){var timerref = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$makeGraphData)(_(RingOscillator$nosc)(RingOscillator$defaultParams)))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return _(_(Fay$$bind)(_(RingOscillator$newRef)(gd)))(function($p1){var gdataref = $p1;return _(_(Fay$$then)(_(_(_(_(RingOscillator$render)(c))(RingOscillator$defaultParams))(RingOscillator$ic))(RingOscillator$renderrng)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(_(_(RingOscillator$centre)(RingOscillator$defaultParams))(RingOscillator$ic)))(RingOscillator$renderrng)))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(_(RingOscillator$addEventListener)(go))(Fay$$list("click"))))(_(_(_(RingOscillator$doGo)(timerref))(_(_(_(_(_(_(RingOscillator$animate)(c))(cg))(pref))(xref))(gdataref))(RingOscillator$renderrng)))(RingOscillator$framems))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(_(RingOscillator$addEventListener)(stop))(Fay$$list("click"))))(_(RingOscillator$doStop)(timerref))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(_(RingOscillator$addEventListener)(reset))(Fay$$list("click"))))(_(_(_(_(_(_(_(RingOscillator$doReset)(c))(cg))(timerref))(pref))(xref))(gdataref))(sels))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$forM_)(_(_(Language$Fay$Stdlib$zip)(sels))(Fay$$list([RingOscillator$alphaUpd,RingOscillator$omegaUpd,RingOscillator$devenUpd,RingOscillator$doddUpd,RingOscillator$betaUpd,RingOscillator$sigmaUpd])))))(function($p1){if (Fay$$listLen(_($p1),2)) {var s = Fay$$index(0)(_($p1));var pfn = Fay$$index(1)(_($p1));return _(_(Language$Fay$Stdlib$$36$)(_(_(RingOscillator$addEventListener)(s))(Fay$$list("change"))))(_(_(RingOscillator$doParamChange)(pref))(pfn));}throw ["unhandled case",$p1];})))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(_(RingOscillator$addEventListener)(noscSel))(Fay$$list("change"))))(_(_(_(_(_(_(RingOscillator$doNoscChange)(c))(cg))(timerref))(pref))(xref))(gdataref))))(_(Fay$$$_return)(false))))))));});});});});});}throw ["unhandled case",$p1];});});});}throw ["unhandled case",$p1];});}throw ["unhandled case",$p1];});});};var RingOscillator$makeGraphData = function($p1){return function($p2){return new $(function(){var size = $p2;var no = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$replicateM)(no))(_(RingOscillator$newBuf)(size))))(function($p1){var bufs = $p1;return _(Fay$$$_return)(Fay$$list([0,bufs]));});});};};var RingOscillator$doGo = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){var interval = $p3;var anim = $p2;var tref = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(tref)))(function($p1){var oldtimer = $p1;return _(_(Fay$$then)((function($tmp1){if (_($tmp1) instanceof $_Language$Fay$Stdlib$Nothing) {return _(_(Fay$$bind)(_(_(RingOscillator$setInterval)(anim))(interval)))(function($p1){var timer = $p1;return _(_(RingOscillator$writeRef)(tref))(_(Language$Fay$Stdlib$Just)(timer));});}if (_($tmp1) instanceof $_Language$Fay$Stdlib$Just) {return _(Fay$$$_return)(Fay$$unit);}return (function(){ throw (["unhandled case",$tmp1]); })();})(oldtimer)))(_(Fay$$$_return)(false));});});};};};};var RingOscillator$doStop = function($p1){return function($p2){return new $(function(){var tref = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(tref)))(function($p1){var oldtimer = $p1;return _(_(Fay$$then)((function($tmp1){if (_($tmp1) instanceof $_Language$Fay$Stdlib$Nothing) {return _(Fay$$$_return)(Fay$$unit);}if (_($tmp1) instanceof $_Language$Fay$Stdlib$Just) {var timer = _($tmp1).slot1;return _(_(Fay$$then)(_(RingOscillator$clearInterval)(timer)))(_(_(RingOscillator$writeRef)(tref))(Language$Fay$Stdlib$Nothing));}return (function(){ throw (["unhandled case",$tmp1]); })();})(oldtimer)))(_(Fay$$$_return)(false));});});};};var RingOscillator$doReset = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return function($p8){return new $(function(){var e = $p8;var sels = $p7;var gdataref = $p6;var xref = $p5;var pref = $p4;var tref = $p3;var cg = $p2;var c = $p1;return _(_(Fay$$then)(_(_(RingOscillator$doStop)(tref))(e)))(_(_(Fay$$then)(_(_(RingOscillator$writeRef)(pref))(RingOscillator$defaultParams)))(_(_(Fay$$then)(_(_(RingOscillator$writeRef)(xref))(RingOscillator$ic)))(_(_(Fay$$bind)(_(_(RingOscillator$makeGraphData)(_(RingOscillator$nosc)(RingOscillator$defaultParams)))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return _(_(Fay$$then)(_(_(RingOscillator$writeRef)(gdataref))(gd)))(_(_(Fay$$then)(_(_(_(_(RingOscillator$render)(c))(RingOscillator$defaultParams))(RingOscillator$ic))(RingOscillator$renderrng)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(_(_(RingOscillator$centre)(RingOscillator$defaultParams))(RingOscillator$ic)))(RingOscillator$renderrng)))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$forM_)(sels)))(function($p1){var s = $p1;return _(_(RingOscillator$setSelectIndex)(s))(2);})))(_(Fay$$$_return)(false)))));}))));});};};};};};};};};var RingOscillator$doParamChange = function($p1){return function($p2){return function($p3){return new $(function(){var e = $p3;var updfn = $p2;var pref = $p1;return _(_(Fay$$bind)(_(RingOscillator$eventTarget)(e)))(function($p1){var target = $p1;return _(_(Fay$$bind)(_(RingOscillator$selectValue)(target)))(function($p1){var sval = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return _(_(Fay$$then)(_(_(RingOscillator$writeRef)(pref))(_(_(updfn)(p))(_(RingOscillator$parseDouble)(sval)))))(_(Fay$$$_return)(false));});});});});};};};var RingOscillator$doNoscChange = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return new $(function(){var e = $p7;var gdataref = $p6;var xref = $p5;var pref = $p4;var timerref = $p3;var cg = $p2;var c = $p1;return _(_(Fay$$then)(_(_(RingOscillator$doStop)(timerref))(e)))(_(_(Fay$$bind)(_(RingOscillator$eventTarget)(e)))(function($p1){var target = $p1;return _(_(Fay$$bind)(_(RingOscillator$selectValue)(target)))(function($p1){var sval = $p1;return (function(){var newNosc = new $(function(){return _(RingOscillator$parseInt)(sval);});return _(_(Fay$$bind)(_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return (function(){var pnew = new $(function(){var $36$_record_to_update = Object.create(_(p));$36$_record_to_update.nosc = newNosc;return $36$_record_to_update;});return (function(){var xnew = new $(function(){return _(_(RingOscillator$centre)(pnew))(_(RingOscillator$initialState)(pnew));});return _(_(Fay$$then)(_(_(RingOscillator$writeRef)(pref))(pnew)))(_(_(Fay$$then)(_(_(RingOscillator$writeRef)(xref))(xnew)))(_(_(Fay$$bind)(_(_(RingOscillator$makeGraphData)(newNosc))(RingOscillator$nsamp)))(function($p1){var gd = $p1;return _(_(Fay$$then)(_(_(RingOscillator$writeRef)(gdataref))(gd)))(_(_(Fay$$then)(_(_(_(_(RingOscillator$render)(c))(pnew))(xnew))(RingOscillator$renderrng)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(xnew))(RingOscillator$renderrng)))(_(Fay$$$_return)(false))));})));})();})();});})();});}));});};};};};};};};var RingOscillator$animate = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return new $(function(){var rng = $p6;var gdataref = $p5;var xref = $p4;var pref = $p3;var cg = $p2;var c = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(xref)))(function($p1){var x = $p1;return (function(){var newx = new $(function(){return _(_(_(RingOscillator$rk4)(_(RingOscillator$rhs)(p)))(x))(RingOscillator$dt);});return _(_(Fay$$then)(_(_(RingOscillator$writeRef)(xref))(newx)))(_(_(Fay$$then)(_(_(_(_(RingOscillator$render)(c))(p))(newx))(rng)))(_(_(_(_(_(RingOscillator$renderGraph)(cg))(pref))(gdataref))(_(_(RingOscillator$centre)(p))(newx)))(rng)));})();});});});};};};};};};var RingOscillator$render = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){var rng = $p4;var s = $p3;var p = $p2;var c = $p1;return (function(){var dtickin = new $(function(){return _(Fay$$sub)(_(RingOscillator$fyoff))(_(_(Fay$$mult)(0.5)(_(RingOscillator$ticklen))));});var dtickout = new $(function(){return _(Fay$$add)(_(RingOscillator$fyoff))(_(_(Fay$$mult)(0.5)(_(RingOscillator$ticklen))));});return (function(){var f = new $(function(){return _(RingOscillator$head)(s);});return (function(){var xs = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$take)(_(RingOscillator$nosc)(p))))(_(RingOscillator$tail)(s));});return _(_(Fay$$then)(_(_(_(RingOscillator$clearRect)(c))(Fay$$list([0,0])))(Fay$$list([RingOscillator$ww,RingOscillator$wh]))))(_(_(Fay$$then)(_(RingOscillator$beginPath)(c)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$arc)(c))(RingOscillator$ctr))(RingOscillator$rinner))(0))(_(Fay$$mult)(2)(_(RingOscillator$pi)))))((function(){var fscale = new $(function(){return _(Fay$$div)(_(_(Fay$$mult)(_(_(Fay$$mult)(_(RingOscillator$rinner))(2)))(_(RingOscillator$pi))))(5);});return _(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$moveTo)(c)))(_(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([(-(_(_(Fay$$mult)(0.5)(_(fscale))))),(-(_(RingOscillator$fyoff)))])))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$lineTo)(c)))(_(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([_(Fay$$mult)(0.5)(_(fscale)),(-(_(RingOscillator$fyoff)))])))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$moveTo)(c)))(_(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([0,(-(_(dtickin)))])))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$lineTo)(c)))(_(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([0,(-(_(dtickout)))])))))(_(_(Fay$$then)(_(_(RingOscillator$setLineWidth)(c))(2)))(_(_(Fay$$then)(_(_(RingOscillator$setStrokeStyle)(c))(Fay$$list("grey"))))(_(_(Fay$$then)(_(RingOscillator$stroke)(c)))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$forM_)(Language$Fay$Stdlib$enumFromTo(0)(_(Fay$$sub)(_(_(RingOscillator$nosc)(p)))(1))))(function($p1){var i = $p1;return (function(){var th0 = new $(function(){return _(Fay$$div)(_(_(Fay$$mult)(_(_(Fay$$mult)(_(_(Language$Fay$Stdlib$fromIntegral)(i)))(2)))(_(RingOscillator$pi))))(_(_(Language$Fay$Stdlib$fromIntegral)(_(RingOscillator$nosc)(p))));});return (function(){var th = new $(function(){return _(Fay$$add)(_(th0))(_(_(Fay$$div)(_(_(Fay$$mult)(_(_(Fay$$mult)(_(_(Fay$$div)(_(_(_(RingOscillator$$33$$33$)(xs))(i)))(_(rng))))(2)))(_(RingOscillator$pi))))(_(_(Language$Fay$Stdlib$fromIntegral)(_(RingOscillator$nosc)(p))))));});return (function(){var dctr = new $(function(){return _(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([_(Fay$$mult)(_(RingOscillator$rinner))(_(_(RingOscillator$sin)(th))),(-(_(_(Fay$$mult)(_(RingOscillator$rinner))(_(_(RingOscillator$cos)(th))))))]));});return _(_(Fay$$then)(_(RingOscillator$beginPath)(c)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$arc)(c))(dctr))(RingOscillator$rdot))(0))(_(Fay$$mult)(2)(_(RingOscillator$pi)))))(_(_(Fay$$then)(_(_(RingOscillator$setFillStyle)(c))(_(_(_(Fay$$eq)(i))(0)) ? Fay$$list("blue") : Fay$$list("red"))))(_(RingOscillator$fill)(c))));})();})();})();})))((function(){var dctr = new $(function(){return _(_(RingOscillator$offset)(RingOscillator$ctr))(Fay$$list([_(Fay$$mult)(_(_(Fay$$div)(_(f))(_(rng))))(_(fscale)),(-(_(RingOscillator$fyoff)))]));});return _(_(Fay$$then)(_(RingOscillator$beginPath)(c)))(_(_(Fay$$then)(_(_(_(_(_(RingOscillator$arc)(c))(dctr))(RingOscillator$rdot))(0))(_(Fay$$mult)(2)(_(RingOscillator$pi)))))(_(_(Fay$$then)(_(_(RingOscillator$setFillStyle)(c))(Fay$$list("grey"))))(_(RingOscillator$fill)(c))));})()))))))));})())));})();})();})();});};};};};var RingOscillator$renderGraph = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new $(function(){var rng = $p5;var x = $p4;var gdataref = $p3;var pref = $p2;var cg = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(pref)))(function($p1){var p = $p1;return _(_(Fay$$bind)(_(RingOscillator$readRef)(gdataref)))(function($p1){if (Fay$$listLen(_($p1),2)) {var ts = Fay$$index(0)(_($p1));var bufs = Fay$$index(1)(_($p1));return _(_(Fay$$then)(_(_(RingOscillator$setFont)(cg))(Fay$$list("10pt sans-serif"))))(_(_(Fay$$then)(_(_(RingOscillator$setStrokeStyle)(cg))(Fay$$list("grey"))))(_(_(Fay$$then)(_(_(RingOscillator$setFillStyle)(cg))(Fay$$list("grey"))))(_(_(Fay$$then)(_(_(RingOscillator$setLineWidth)(cg))(2)))(_(_(Fay$$then)(_(_(_(RingOscillator$clearRect)(cg))(Fay$$list([0,0])))(Fay$$list([RingOscillator$gww,RingOscillator$gwh]))))(_(_(Fay$$then)(_(RingOscillator$beginPath)(cg)))(_(_(Fay$$then)(_(_(RingOscillator$moveTo)(cg))(Fay$$list([0,_(Fay$$div)(_(RingOscillator$gwh))(2)]))))(_(_(Fay$$then)(_(_(RingOscillator$lineTo)(cg))(Fay$$list([RingOscillator$gww,_(Fay$$div)(_(RingOscillator$gwh))(2)]))))(_(_(Fay$$then)(_(_(RingOscillator$moveTo)(cg))(Fay$$list([1,0]))))(_(_(Fay$$then)(_(_(RingOscillator$lineTo)(cg))(Fay$$list([1,RingOscillator$gwh]))))((function(){var ticks = new $(function(){return _(_(RingOscillator$takeWhile)(function($p1){var t = $p1;return _(_(Fay$$lte)(t))(_(RingOscillator$floor)(_(Fay$$add)(_(ts))(_(RingOscillator$gwwtime))));}))(Language$Fay$Stdlib$enumFrom(_(Fay$$add)(_(_(RingOscillator$floor)(ts)))(1)));});return (function(){var tickxs = new $(function(){return _(_(Language$Fay$Stdlib$map)(function($p1){var t = $p1;return _(Fay$$mult)(_(_(Fay$$sub)(_(_(Language$Fay$Stdlib$fromIntegral)(t)))(_(ts))))(_(RingOscillator$pxpert));}))(ticks);});return _(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$forM_)(_(_(Language$Fay$Stdlib$zip)(ticks))(tickxs))))(function($p1){if (Fay$$listLen(_($p1),2)) {var t = Fay$$index(0)(_($p1));var x = Fay$$index(1)(_($p1));return _(_(Fay$$then)(_(_(RingOscillator$moveTo)(cg))(Fay$$list([x,_(Fay$$sub)(_(_(Fay$$div)(_(RingOscillator$gwh))(2)))(_(_(Fay$$div)(_(RingOscillator$ticklen))(2)))]))))(_(_(Fay$$then)(_(_(RingOscillator$lineTo)(cg))(Fay$$list([x,_(Fay$$add)(_(_(Fay$$div)(_(RingOscillator$gwh))(2)))(_(_(Fay$$div)(_(RingOscillator$ticklen))(2)))]))))((function(){var txt = new $(function(){return _(Language$Fay$Stdlib$show)(t);});return _(_(Fay$$bind)(_(_(RingOscillator$measureText)(cg))(txt)))(function($p1){var txtw = $p1;return _(_(_(_(RingOscillator$fillText)(cg))(txt))(Fay$$list([_(Fay$$sub)(_(x))(_(_(Fay$$div)(_(txtw))(2))),_(Fay$$add)(_(_(Fay$$div)(_(RingOscillator$gwh))(2)))(_(_(Fay$$mult)(2)(_(RingOscillator$ticklen))))])))(Language$Fay$Stdlib$Nothing);});})()));}throw ["unhandled case",$p1];})))(_(_(Fay$$then)(_(RingOscillator$stroke)(cg)))(_(_(Fay$$then)(_(_(RingOscillator$setLineWidth)(cg))(1)))(_(_(Fay$$bind)(_(_(Language$Fay$Stdlib$$36$)(_(RingOscillator$forM)(_(_(_(RingOscillator$zip3)(x))(bufs))(_(_(Fay$$cons)(Fay$$list("grey")))(_(_(Fay$$cons)(Fay$$list("blue")))(_(RingOscillator$repeat)(Fay$$list("red"))))))))(function($p1){if (Fay$$listLen(_($p1),3)) {var newx = Fay$$index(0)(_($p1));var buf = Fay$$index(1)(_($p1));var col = Fay$$index(2)(_($p1));return _(_(Fay$$bind)(_(_(RingOscillator$bufAdd)(buf))(newx)))(function($p1){var newbuf = $p1;return _(_(Fay$$then)(_(RingOscillator$beginPath)(cg)))(_(_(Fay$$then)(_(_(RingOscillator$setStrokeStyle)(cg))(col)))(_(_(Fay$$bind)(_(_(RingOscillator$bufVal)(newbuf))(0)))(function($p1){var y0 = $p1;return _(_(Fay$$then)(_(_(RingOscillator$moveTo)(cg))(Fay$$list([0,_(Fay$$mult)(_(_(Fay$$div)(_(RingOscillator$gwh))(2)))(_(_(Fay$$sub)(1)(_(_(Fay$$div)(_(y0))(_(RingOscillator$renderrng))))))]))))(_(_(Fay$$then)(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$when)(_(Fay$$gt)(_(_(RingOscillator$bufCurSize)(buf)))(1))))(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$forM_)(Language$Fay$Stdlib$enumFromTo(1)(_(Fay$$sub)(_(_(RingOscillator$bufCurSize)(buf)))(1)))))(function($p1){var i = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$bufVal)(newbuf))(i)))(function($p1){var y = $p1;return _(_(RingOscillator$lineTo)(cg))(Fay$$list([_(Fay$$mult)(_(_(Language$Fay$Stdlib$fromIntegral)(i)))(_(RingOscillator$pxpersamp)),_(Fay$$mult)(_(_(Fay$$div)(_(RingOscillator$gwh))(2)))(_(_(Fay$$sub)(1)(_(_(Fay$$div)(_(y))(_(RingOscillator$renderrng))))))]));});}))))(_(_(Fay$$then)(_(RingOscillator$stroke)(cg)))(_(Fay$$$_return)(newbuf))));})));});}throw ["unhandled case",$p1];})))(function($p1){var newbufs = $p1;return _(_(RingOscillator$writeRef)(gdataref))(Fay$$list([_(Fay$$add)(_(ts))(_(_(_(Fay$$lt)(_(_(RingOscillator$bufCurSize)(_(RingOscillator$head)(newbufs))))(_(RingOscillator$nsamp))) ? 0 : RingOscillator$dt)),newbufs]));}))));})();})()))))))))));}throw ["unhandled case",$p1];});});});};};};};};var RingOscillator$pi = new $(function(){return Fay$$jsToFay(["double"],Math.PI);});var RingOscillator$sin = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.sin(Fay$$fayToJs(["double"],$p1)));});};var RingOscillator$cos = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.cos(Fay$$fayToJs(["double"],$p1)));});};var RingOscillator$replicate = function($p1){return function($p2){return new $(function(){if (_($p1) === 0) {return null;}var x = $p2;var n = $p1;return _(_(Fay$$cons)(x))(_(_(RingOscillator$replicate)(_(Fay$$sub)(_(n))(1)))(x));});};};var RingOscillator$take = function($p1){return function($p2){return new $(function(){var n = $p1;if (_(_(_(Fay$$lte)(n))(0))) {return null;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$cons)(x))(_(_(RingOscillator$take)(_(Fay$$sub)(_(n))(1)))(xs));}});};};var RingOscillator$drop = function($p1){return function($p2){return new $(function(){var xs = $p2;var n = $p1;if (_(_(_(Fay$$lte)(n))(0))) {return xs;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return _(_(RingOscillator$drop)(_(Fay$$sub)(_(n))(1)))(xs);}});};};var RingOscillator$splitAt = function($p1){return function($p2){return new $(function(){var xs = $p2;var n = $p1;return Fay$$list([_(_(RingOscillator$take)(n))(xs),_(_(RingOscillator$drop)(n))(xs)]);});};};var RingOscillator$takeWhile = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;if (_(_(p)(x))) {return _(_(Fay$$cons)(x))(_(_(RingOscillator$takeWhile)(p))(xs));} else {if (true) {return null;}}}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};var RingOscillator$$94$ = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {return 1;}var n = $p2;var x = $p1;return _(Fay$$mult)(_(x))(_(_(_(RingOscillator$$94$)(x))(_(Fay$$sub)(_(n))(1))));});};};var RingOscillator$head = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;return x;}throw ["unhandled case in head",[$p1]];});};var RingOscillator$tail = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;return xs;}throw ["unhandled case in tail",[$p1]];});};var RingOscillator$zip3 = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b,c])))(_(_(_(RingOscillator$zip3)(as))(bs))(cs));}}}return null;});};};};var RingOscillator$zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){var $tmp1 = _($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var z = $p1;return _(_(Fay$$cons)(_(_(_(z)(a))(b))(c)))(_(_(_(_(RingOscillator$zipWith3)(z))(as))(bs))(cs));}}}return null;});};};};};var RingOscillator$zipWith5 = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return new $(function(){var $tmp1 = _($p6);if ($tmp1 instanceof Fay$$Cons) {var e = $tmp1.car;var es = $tmp1.cdr;var $tmp1 = _($p5);if ($tmp1 instanceof Fay$$Cons) {var d = $tmp1.car;var ds = $tmp1.cdr;var $tmp1 = _($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var z = $p1;return _(_(Fay$$cons)(_(_(_(_(_(z)(a))(b))(c))(d))(e)))(_(_(_(_(_(_(RingOscillator$zipWith5)(z))(as))(bs))(cs))(ds))(es));}}}}}return null;});};};};};};};var RingOscillator$$33$$33$ = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;return x;}}var n = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;return _(_(RingOscillator$$33$$33$)(xs))(_(Fay$$sub)(_(n))(1));}throw ["unhandled case in (!!)",[$p1,$p2]];});};};var RingOscillator$mapM = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return _(_(Fay$$bind)(_(m)(x)))(function($p1){var mx = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(m))(xs)))(function($p1){var mxs = $p1;return _(Fay$$$_return)(_(_(Fay$$cons)(mx))(mxs));});});}if (_($p2) === null) {return _(Fay$$$_return)(null);}throw ["unhandled case in mapM",[$p1,$p2]];});};};var RingOscillator$forM = function($p1){return function($p2){return new $(function(){var m = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$bind)(_(m)(x)))(function($p1){var mx = $p1;return _(_(Fay$$bind)(_(_(RingOscillator$mapM)(m))(xs)))(function($p1){var mxs = $p1;return _(Fay$$$_return)(_(_(Fay$$cons)(mx))(mxs));});});}if (_($p1) === null) {return _(Fay$$$_return)(null);}throw ["unhandled case in forM",[$p1,$p2]];});};};var RingOscillator$replicateM = function($p1){return function($p2){return new $(function(){var x = $p2;var n = $p1;return _(Language$Fay$Stdlib$sequence)(_(_(RingOscillator$replicate)(n))(x));});};};var RingOscillator$sum = function($p1){return new $(function(){var l = $p1;return (function(){var sum$39$ = function($p1){return function($p2){return new $(function(){var a = $p2;if (_($p1) === null) {return a;}var a = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(sum$39$)(xs))(_(Fay$$add)(_(a))(_(x)));}throw ["unhandled case in sum'",[$p1,$p2]];});};};return _(_(sum$39$)(l))(0);})();});};var RingOscillator$cycle = function($p1){return new $(function(){var xs = $p1;return (function(){var xs$39$ = new $(function(){return _(_(Language$Fay$Stdlib$$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};var RingOscillator$repeat = function($p1){return new $(function(){var x = $p1;return (function(){var xs = new $(function(){return _(_(Fay$$cons)(x))(xs);});return xs;})();});};var RingOscillator$parseDouble = function($p1){return new $(function(){return Fay$$jsToFay(["double"],parseFloat(Fay$$fayToJs(["string"],$p1)));});};var RingOscillator$parseInt = function($p1){return new $(function(){return Fay$$jsToFay(["int"],parseInt(Fay$$fayToJs(["string"],$p1)));});};var RingOscillator$getElementById = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document['getElementById'](Fay$$fayToJs(["string"],$p1)));});};var RingOscillator$addWindowEventListener = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['addEventListener'](Fay$$fayToJs(["string"],$p1),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p2),false));});};};var RingOscillator$addEventListener = function($p1){return function($p2){return function($p3){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Element",[]],$p1)['addEventListener'](Fay$$fayToJs(["string"],$p2),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p3),false));});};};};var RingOscillator$setInterval = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["int"]]],window['setInterval'](Fay$$fayToJs(["action",[["unknown"]]],$p1),Fay$$fayToJs(["double"],$p2)));});};};var RingOscillator$clearInterval = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['clearInterval'](Fay$$fayToJs(["int"],$p1)));});};var RingOscillator$print = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],console.log(Fay$$fayToJs(["unknown"],$p1)));});};var RingOscillator$eventTarget = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],Fay$$fayToJs(["user","Event",[]],$p1)['target']);});};var RingOscillator$selectValue = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","Element",[]],$p1)[Fay$$fayToJs(["user","Element",[]],$p1)['selectedIndex']]['value']);});};var RingOscillator$setSelectIndex = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Element",[]],$p1)['selectedIndex']=Fay$$fayToJs(["int"],$p2));});};};var RingOscillator$floor = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.floor(Fay$$fayToJs(["double"],$p1)));});};var RingOscillator$newRef = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Ref",[["unknown"]]]]],new Fay$$Ref(Fay$$fayToJs(["unknown"],$p1)));});};var RingOscillator$writeRef = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$writeRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var RingOscillator$readRef = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$readRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1)));});};var RingOscillator$getContext = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["user","Context",[]]]],Fay$$fayToJs(["user","Element",[]],$p1).getContext(Fay$$fayToJs(["string"],$p2)));});};};var RingOscillator$offset = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var dx = Fay$$index(0)(_($p2));var dy = Fay$$index(1)(_($p2));if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(0)(_($p1));var y = Fay$$index(1)(_($p1));return Fay$$list([_(Fay$$add)(_(x))(_(dx)),_(Fay$$add)(_(y))(_(dy))]);}}throw ["unhandled case in offset",[$p1,$p2]];});};};var RingOscillator$setFillStyle = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fillStyle']=Fay$$fayToJs(["string"],$p2));});};};var RingOscillator$setFont = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['font']=Fay$$fayToJs(["string"],$p2));});};};var RingOscillator$setLineWidth = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['lineWidth']=Fay$$fayToJs(["double"],$p2));});};};var RingOscillator$setStrokeStyle = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['strokeStyle']=Fay$$fayToJs(["string"],$p2));});};};var RingOscillator$arc = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new $(function(){var end = $p5;var beg = $p4;var r = $p3;if (Fay$$listLen(_($p2),2)) {var x = Fay$$index(0)(_($p2));var y = Fay$$index(1)(_($p2));var c = $p1;return _(_(_(_(_(_(_(RingOscillator$arc$39$)(c))(x))(y))(r))(beg))(end))(true);}throw ["unhandled case in arc",[$p1,$p2,$p3,$p4,$p5]];});};};};};};var RingOscillator$arcC = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new $(function(){var end = $p5;var beg = $p4;var r = $p3;if (Fay$$listLen(_($p2),2)) {var x = Fay$$index(0)(_($p2));var y = Fay$$index(1)(_($p2));var c = $p1;return _(_(_(_(_(_(_(RingOscillator$arc$39$)(c))(x))(y))(r))(beg))(end))(false);}throw ["unhandled case in arcC",[$p1,$p2,$p3,$p4,$p5]];});};};};};};var RingOscillator$arc$39$ = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return function($p7){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['arc'](Fay$$fayToJs(["double"],$p2),Fay$$fayToJs(["double"],$p3),Fay$$fayToJs(["double"],$p4),Fay$$fayToJs(["double"],$p5),Fay$$fayToJs(["double"],$p6),Fay$$fayToJs(["bool"],$p7)));});};};};};};};};var RingOscillator$beginPath = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['beginPath']());});};var RingOscillator$clip = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['clip']());});};var RingOscillator$closePath = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['closePath']());});};var RingOscillator$fill = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fill']());});};var RingOscillator$lineTo = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var x = Fay$$index(0)(_($p2));var y = Fay$$index(1)(_($p2));var c = $p1;return _(_(_(RingOscillator$lineTo$39$)(c))(x))(y);}throw ["unhandled case in lineTo",[$p1,$p2]];});};};var RingOscillator$lineTo$39$ = function($p1){return function($p2){return function($p3){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['lineTo'](Fay$$fayToJs(["double"],$p2),Fay$$fayToJs(["double"],$p3)));});};};};var RingOscillator$moveTo = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var x = Fay$$index(0)(_($p2));var y = Fay$$index(1)(_($p2));var c = $p1;return _(_(_(RingOscillator$moveTo$39$)(c))(x))(y);}throw ["unhandled case in moveTo",[$p1,$p2]];});};};var RingOscillator$moveTo$39$ = function($p1){return function($p2){return function($p3){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['moveTo'](Fay$$fayToJs(["double"],$p2),Fay$$fayToJs(["double"],$p3)));});};};};var RingOscillator$stroke = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['stroke']());});};var RingOscillator$clearRect = function($p1){return function($p2){return function($p3){return new $(function(){if (Fay$$listLen(_($p3),2)) {var w = Fay$$index(0)(_($p3));var h = Fay$$index(1)(_($p3));if (Fay$$listLen(_($p2),2)) {var x = Fay$$index(0)(_($p2));var y = Fay$$index(1)(_($p2));var c = $p1;return _(_(_(_(_(RingOscillator$clearRect$39$)(c))(x))(y))(w))(h);}}throw ["unhandled case in clearRect",[$p1,$p2,$p3]];});};};};var RingOscillator$clearRect$39$ = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['clearRect'](Fay$$fayToJs(["double"],$p2),Fay$$fayToJs(["double"],$p3),Fay$$fayToJs(["double"],$p4),Fay$$fayToJs(["double"],$p5)));});};};};};};var RingOscillator$fillText = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){if (_($p4) instanceof $_Language$Fay$Stdlib$Nothing) {if (Fay$$listLen(_($p3),2)) {var x = Fay$$index(0)(_($p3));var y = Fay$$index(1)(_($p3));var s = $p2;var c = $p1;return _(_(_(_(RingOscillator$fillText1)(c))(s))(x))(y);}}if (_($p4) instanceof $_Language$Fay$Stdlib$Just) {var mw = _($p4).slot1;if (Fay$$listLen(_($p3),2)) {var x = Fay$$index(0)(_($p3));var y = Fay$$index(1)(_($p3));var s = $p2;var c = $p1;return _(_(_(_(_(RingOscillator$fillText2)(c))(s))(x))(y))(mw);}}throw ["unhandled case in fillText",[$p1,$p2,$p3,$p4]];});};};};};var RingOscillator$fillText1 = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fillText'](Fay$$fayToJs(["string"],$p2),Fay$$fayToJs(["double"],$p3),Fay$$fayToJs(["double"],$p4)));});};};};};var RingOscillator$fillText2 = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fillText'](Fay$$fayToJs(["string"],$p2),Fay$$fayToJs(["double"],$p3),Fay$$fayToJs(["double"],$p4),Fay$$fayToJs(["double"],$p5)));});};};};};};var RingOscillator$measureText = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Context",[]],$p1)['measureText'](Fay$$fayToJs(["string"],$p2))['width']);});};};var $_RingOscillator$Buffer = function(bufSize,bufCurSize,bufNext,bufArr){this.bufSize = bufSize;this.bufCurSize = bufCurSize;this.bufNext = bufNext;this.bufArr = bufArr;};var RingOscillator$Buffer = function(bufSize){return function(bufCurSize){return function(bufNext){return function(bufArr){return new $(function(){return new $_RingOscillator$Buffer(bufSize,bufCurSize,bufNext,bufArr);});};};};};var RingOscillator$bufSize = function(x){return new $(function(){return _(x).bufSize;});};var RingOscillator$bufCurSize = function(x){return new $(function(){return _(x).bufCurSize;});};var RingOscillator$bufNext = function(x){return new $(function(){return _(x).bufNext;});};var RingOscillator$bufArr = function(x){return new $(function(){return _(x).bufArr;});};var RingOscillator$newBuf = function($p1){return new $(function(){var size = $p1;return _(_(Fay$$bind)(_(RingOscillator$newArray)(size)))(function($p1){var arr = $p1;return _(_(Language$Fay$Stdlib$$36$)(Fay$$$_return))(_(_(_(_(RingOscillator$Buffer)(size))(0))(0))(arr));});});};var RingOscillator$bufAdd = function($p1){return function($p2){return new $(function(){var x = $p2;if (_($p1) instanceof $_RingOscillator$Buffer) {var sz = _($p1).bufSize;var cursz = _($p1).bufCurSize;var nxt = _($p1).bufNext;var arr = _($p1).bufArr;return (function(){var cursz$39$ = new $(function(){return _(_(Fay$$lt)(_(cursz))(_(sz))) ? _(Fay$$add)(_(cursz))(1) : sz;});return _(_(Fay$$then)(_(_(_(RingOscillator$setArrayVal)(arr))(nxt))(x)))((function(){var nxt$39$ = new $(function(){return _(_(RingOscillator$rem)(_(Fay$$add)(_(nxt))(1)))(sz);});return _(_(Language$Fay$Stdlib$$36$)(Fay$$$_return))(_(_(_(_(RingOscillator$Buffer)(sz))(cursz$39$))(nxt$39$))(arr));})());})();}throw ["unhandled case in bufAdd",[$p1,$p2]];});};};var RingOscillator$bufVal = function($p1){return function($p2){return new $(function(){var i = $p2;if (_($p1) instanceof $_RingOscillator$Buffer) {var sz = _($p1).bufSize;var cursz = _($p1).bufCurSize;var nxt = _($p1).bufNext;var arr = _($p1).bufArr;return (function(){var idx = new $(function(){return _(_(RingOscillator$rem)(_(_(Fay$$lt)(_(cursz))(_(sz))) ? i : _(Fay$$add)(_(nxt))(_(i))))(sz);});return _(_(Fay$$bind)(_(_(RingOscillator$arrayVal)(arr))(idx)))(Fay$$$_return);})();}throw ["unhandled case in bufVal",[$p1,$p2]];});};};var RingOscillator$newArray = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Array",[]]]],new Array(Fay$$fayToJs(["int"],$p1)));});};var RingOscillator$setArrayVal = function($p1){return function($p2){return function($p3){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Array",[]],$p1)[Fay$$fayToJs(["int"],$p2)]=Fay$$fayToJs(["double"],$p3));});};};};var RingOscillator$arrayVal = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Array",[]],$p1)[Fay$$fayToJs(["int"],$p2)]);});};};var RingOscillator$rem = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["int"],Fay$$fayToJs(["int"],$p1) % Fay$$fayToJs(["int"],$p2));});};};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = _(obj);var argTypes = type[2];if (_obj instanceof $_RingOscillator$Buffer) {return {"instance": "Buffer","bufSize": Fay$$fayToJs(["int"],_(_obj.bufSize)),"bufCurSize": Fay$$fayToJs(["int"],_(_obj.bufCurSize)),"bufNext": Fay$$fayToJs(["int"],_(_obj.bufNext)),"bufArr": Fay$$fayToJs(["user","Array",[]],_(_obj.bufArr))};}if (_obj instanceof $_RingOscillator$Params) {return {"instance": "Params","alpha": Fay$$fayToJs(["double"],_(_obj.alpha)),"omega": Fay$$fayToJs(["double"],_(_obj.omega)),"deven": Fay$$fayToJs(["double"],_(_obj.deven)),"dodd": Fay$$fayToJs(["double"],_(_obj.dodd)),"beta": Fay$$fayToJs(["double"],_(_obj.beta)),"sigma": Fay$$fayToJs(["double"],_(_obj.sigma)),"nosc": Fay$$fayToJs(["int"],_(_obj.nosc))};}if (_obj instanceof $_Language$Fay$Stdlib$Nothing) {return {"instance": "Nothing"};}if (_obj instanceof $_Language$Fay$Stdlib$Just) {return {"instance": "Just","slot1": Fay$$fayToJs(["unknown"],_(_obj.slot1))};}if (_obj instanceof $_Language$Fay$Stdlib$EQ) {return {"instance": "EQ"};}if (_obj instanceof $_Language$Fay$Stdlib$LT) {return {"instance": "LT"};}if (_obj instanceof $_Language$Fay$Stdlib$GT) {return {"instance": "GT"};}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){if (obj["instance"] === "Buffer") {return new $_RingOscillator$Buffer(Fay$$jsToFay(["int"],obj["bufSize"]),Fay$$jsToFay(["int"],obj["bufCurSize"]),Fay$$jsToFay(["int"],obj["bufNext"]),Fay$$jsToFay(["user","Array",[]],obj["bufArr"]));}if (obj["instance"] === "Params") {return new $_RingOscillator$Params(Fay$$jsToFay(["double"],obj["alpha"]),Fay$$jsToFay(["double"],obj["omega"]),Fay$$jsToFay(["double"],obj["deven"]),Fay$$jsToFay(["double"],obj["dodd"]),Fay$$jsToFay(["double"],obj["beta"]),Fay$$jsToFay(["double"],obj["sigma"]),Fay$$jsToFay(["int"],obj["nosc"]));}if (obj["instance"] === "Nothing") {return new $_Language$Fay$Stdlib$Nothing();}if (obj["instance"] === "Just") {return new $_Language$Fay$Stdlib$Just(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "EQ") {return new $_Language$Fay$Stdlib$EQ();}if (obj["instance"] === "LT") {return new $_Language$Fay$Stdlib$LT();}if (obj["instance"] === "GT") {return new $_Language$Fay$Stdlib$GT();}return obj;};
// Exports
this.RingOscillator$main = RingOscillator$main;
this.RingOscillator$Buffer = RingOscillator$Buffer;
this.RingOscillator$Params = RingOscillator$Params;

// Built-ins
this._ = _;
this.$           = $;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new RingOscillator();
main._(main.RingOscillator$main);

