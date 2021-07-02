(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2(elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}var author$project$Common$AtEntrance = F2(
	function (a, b) {
		return {$: 'AtEntrance', a: a, b: b};
	});
var author$project$Common$Information = {$: 'Information'};
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var author$project$Main$makeWebsocketOrigin = function (httpOrigin) {
	return A2(elm$core$String$startsWith, 'http://', httpOrigin) ? ('ws://' + A2(elm$core$String$dropLeft, 7, httpOrigin)) : (A2(elm$core$String$startsWith, 'https://', httpOrigin) ? ('ws://' + A2(elm$core$String$dropLeft, 8, httpOrigin)) : 'ws://this-cannot-happen');
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Models$decodeOption = function (x) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (temp) {
			switch (temp) {
				case 'None':
					return elm$json$Json$Decode$succeed(elm$core$Maybe$Nothing);
				case 'Some':
					return A2(
						elm$json$Json$Decode$field,
						'_arg',
						A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, x));
				default:
					var other = temp;
					return elm$json$Json$Decode$fail(other);
			}
		},
		A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
};
var author$project$Models$decodeString = elm$json$Json$Decode$string;
var author$project$Models$decodeRoomId = author$project$Models$decodeString;
var author$project$Models$decodeUserId = author$project$Models$decodeString;
var author$project$Models$decodeUserName = author$project$Models$decodeString;
var author$project$Models$decodeFlagUser = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyBelongsTo) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyId) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeyName) {
						return elm$json$Json$Decode$succeed(
							{belongsTo: localKeyBelongsTo, id: localKeyId, name: localKeyName});
					},
					A2(elm$json$Json$Decode$field, 'name', author$project$Models$decodeUserName));
			},
			A2(elm$json$Json$Decode$field, 'id', author$project$Models$decodeUserId));
	},
	A2(
		elm$json$Json$Decode$field,
		'belongs_to',
		author$project$Models$decodeOption(author$project$Models$decodeRoomId)));
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Port$listenWebSocket = _Platform_outgoingPort('listenWebSocket', elm$json$Json$Encode$string);
var author$project$WebSocketClient$listen = F2(
	function (origin, userId) {
		return author$project$Port$listenWebSocket(origin + ('/websocket/' + userId));
	});
var elm$core$Debug$log = _Debug_log;
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var author$project$Main$init = function (flag) {
	var wsOrigin = author$project$Main$makeWebsocketOrigin(flag.httpOrigin);
	var maybeFlagUser = function () {
		var _n4 = A2(
			elm$json$Json$Decode$decodeString,
			author$project$Models$decodeOption(author$project$Models$decodeFlagUser),
			flag.user);
		if (_n4.$ === 'Ok') {
			var maybeFlagUser0 = _n4.a;
			return maybeFlagUser0;
		} else {
			var err = _n4.a;
			var _n5 = A2(elm$core$Debug$log, 'failed to parse', err);
			return elm$core$Maybe$Nothing;
		}
	}();
	var _n0 = function () {
		if (maybeFlagUser.$ === 'Nothing') {
			var _n2 = A2(elm$core$Debug$log, 'no user', _Utils_Tuple0);
			return _Utils_Tuple2(
				elm$core$Platform$Cmd$none,
				A2(author$project$Common$AtEntrance, '', elm$core$Maybe$Nothing));
		} else {
			var flagUser = maybeFlagUser.a;
			var _n3 = A2(elm$core$Debug$log, 'flag user', flagUser);
			var userId = flagUser.id;
			var userName = flagUser.name;
			var cmd0 = A2(author$project$WebSocketClient$listen, wsOrigin, userId);
			var user = {userId: userId, userName: userName};
			return _Utils_Tuple2(
				cmd0,
				A2(
					author$project$Common$AtEntrance,
					'',
					elm$core$Maybe$Just(
						_Utils_Tuple2(user, flagUser.belongsTo))));
		}
	}();
	var cmd = _n0.a;
	var state = _n0.b;
	var model = {
		message: _Utils_Tuple2(
			author$project$Common$Information,
			'flag user: ' + (flag.user + (', window width: ' + elm$core$String$fromInt(flag.windowWidth)))),
		origin: flag.httpOrigin,
		state: state,
		window: {height: flag.windowHeight, width: flag.windowWidth}
	};
	return _Utils_Tuple2(model, cmd);
};
var author$project$Common$Heartbeat = {$: 'Heartbeat'};
var author$project$Common$WindowResized = F2(
	function (a, b) {
		return {$: 'WindowResized', a: a, b: b};
	});
var author$project$Constants$heartbeatIntervalMs = 10000.0;
var author$project$Common$OpenWebSocket = function (a) {
	return {$: 'OpenWebSocket', a: a};
};
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Port$onOpenWebSocket = _Platform_incomingPort('onOpenWebSocket', elm$json$Json$Decode$value);
var author$project$WebSocketClient$onOpen = author$project$Port$onOpenWebSocket(
	function (ws) {
		return author$project$Common$OpenWebSocket(ws);
	});
var author$project$Common$ReceiveNotification = function (a) {
	return {$: 'ReceiveNotification', a: a};
};
var author$project$Models$NotifyComment = function (a) {
	return {$: 'NotifyComment', a: a};
};
var author$project$Models$NotifyEntered = function (a) {
	return {$: 'NotifyEntered', a: a};
};
var author$project$Models$NotifyExited = function (a) {
	return {$: 'NotifyExited', a: a};
};
var author$project$Models$NotifyGameStart = function (a) {
	return {$: 'NotifyGameStart', a: a};
};
var author$project$Models$NotifyNextStep = {$: 'NotifyNextStep'};
var author$project$Models$NotifySubmission = function (a) {
	return {$: 'NotifySubmission', a: a};
};
var author$project$Models$decodeUser = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyUserId) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyUserName) {
				return elm$json$Json$Decode$succeed(
					{userId: localKeyUserId, userName: localKeyUserName});
			},
			A2(elm$json$Json$Decode$field, 'user_name', author$project$Models$decodeUserName));
	},
	A2(elm$json$Json$Decode$field, 'user_id', author$project$Models$decodeUserId));
var author$project$Models$decodeComment = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyFrom) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyText) {
				return elm$json$Json$Decode$succeed(
					{from: localKeyFrom, text: localKeyText});
			},
			A2(elm$json$Json$Decode$field, 'text', author$project$Models$decodeString));
	},
	A2(elm$json$Json$Decode$field, 'from', author$project$Models$decodeUser));
var elm$json$Json$Decode$bool = _Json_decodeBool;
var author$project$Models$decodeBool = elm$json$Json$Decode$bool;
var elm$json$Json$Decode$int = _Json_decodeInt;
var author$project$Models$decodeInt = elm$json$Json$Decode$int;
var author$project$Models$decodeGamePlayer = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyScore) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyUser) {
				return elm$json$Json$Decode$succeed(
					{score: localKeyScore, user: localKeyUser});
			},
			A2(elm$json$Json$Decode$field, 'user', author$project$Models$decodeUser));
	},
	A2(elm$json$Json$Decode$field, 'score', author$project$Models$decodeInt));
var author$project$Models$decodePerSeat = function (localParamA) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (localKeyEast) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (localKeyNorth) {
					return A2(
						elm$json$Json$Decode$andThen,
						function (localKeySouth) {
							return A2(
								elm$json$Json$Decode$andThen,
								function (localKeyWest) {
									return elm$json$Json$Decode$succeed(
										{east: localKeyEast, north: localKeyNorth, south: localKeySouth, west: localKeyWest});
								},
								A2(elm$json$Json$Decode$field, 'west', localParamA));
						},
						A2(elm$json$Json$Decode$field, 'south', localParamA));
				},
				A2(elm$json$Json$Decode$field, 'north', localParamA));
		},
		A2(elm$json$Json$Decode$field, 'east', localParamA));
};
var author$project$Models$decodeSeat = author$project$Models$decodeInt;
var author$project$Models$decodeGameMeta = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyInningIndex) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyNumConsecutives) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeyParentSeat) {
						return A2(
							elm$json$Json$Decode$andThen,
							function (localKeyPlayers) {
								return elm$json$Json$Decode$succeed(
									{inningIndex: localKeyInningIndex, numConsecutives: localKeyNumConsecutives, parentSeat: localKeyParentSeat, players: localKeyPlayers});
							},
							A2(
								elm$json$Json$Decode$field,
								'players',
								author$project$Models$decodePerSeat(author$project$Models$decodeGamePlayer)));
					},
					A2(elm$json$Json$Decode$field, 'parent_seat', author$project$Models$decodeSeat));
			},
			A2(elm$json$Json$Decode$field, 'num_consecutives', author$project$Models$decodeInt));
	},
	A2(elm$json$Json$Decode$field, 'inning_index', author$project$Models$decodeInt));
var author$project$Models$ObservableDuringInning = function (a) {
	return {$: 'ObservableDuringInning', a: a};
};
var author$project$Models$ObservableInningEnd = function (a) {
	return {$: 'ObservableInningEnd', a: a};
};
var author$project$Models$Wen = function (a) {
	return {$: 'Wen', a: a};
};
var author$project$Models$Wu = function (a) {
	return {$: 'Wu', a: a};
};
var author$project$Models$decodeCardWen = author$project$Models$decodeInt;
var author$project$Models$decodeCardWuNumber = author$project$Models$decodeInt;
var author$project$Models$decodeDesign = author$project$Models$decodeBool;
var author$project$Models$decodeCardWu = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyDesign) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyNumber) {
				return elm$json$Json$Decode$succeed(
					{design: localKeyDesign, number: localKeyNumber});
			},
			A2(elm$json$Json$Decode$field, 'number', author$project$Models$decodeCardWuNumber));
	},
	A2(elm$json$Json$Decode$field, 'design', author$project$Models$decodeDesign));
var author$project$Models$decodeCard = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'Wen':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$Wen, author$project$Models$decodeCardWen));
			case 'Wu':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$Wu, author$project$Models$decodeCardWu));
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$Models$decodeList = elm$json$Json$Decode$list;
var author$project$Models$Starting = {$: 'Starting'};
var author$project$Models$TrickDoubleBoth = function (a) {
	return {$: 'TrickDoubleBoth', a: a};
};
var author$project$Models$TrickDoubleWen = function (a) {
	return {$: 'TrickDoubleWen', a: a};
};
var author$project$Models$TrickDoubleWu = function (a) {
	return {$: 'TrickDoubleWu', a: a};
};
var author$project$Models$TrickQuadruple = function (a) {
	return {$: 'TrickQuadruple', a: a};
};
var author$project$Models$TrickSingleWen = function (a) {
	return {$: 'TrickSingleWen', a: a};
};
var author$project$Models$TrickSingleWu = function (a) {
	return {$: 'TrickSingleWu', a: a};
};
var author$project$Models$TrickTripleWen = function (a) {
	return {$: 'TrickTripleWen', a: a};
};
var author$project$Models$TrickTripleWu = function (a) {
	return {$: 'TrickTripleWu', a: a};
};
var author$project$Models$TrickWenzun = function (a) {
	return {$: 'TrickWenzun', a: a};
};
var author$project$Models$TrickWuzun = function (a) {
	return {$: 'TrickWuzun', a: a};
};
var author$project$Models$decodeCardBig = author$project$Models$decodeInt;
var author$project$Models$decodeBigWithDesign = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyDesign) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyMain) {
				return elm$json$Json$Decode$succeed(
					{design: localKeyDesign, main: localKeyMain});
			},
			A2(elm$json$Json$Decode$field, 'main', author$project$Models$decodeCardBig));
	},
	A2(elm$json$Json$Decode$field, 'design', author$project$Models$decodeDesign));
var author$project$Models$Closed = {$: 'Closed'};
var author$project$Models$Open = function (a) {
	return {$: 'Open', a: a};
};
var author$project$Models$decodeClosedOr = function (localParamA) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (temp) {
			switch (temp) {
				case 'Closed':
					return elm$json$Json$Decode$succeed(author$project$Models$Closed);
				case 'Open':
					return A2(
						elm$json$Json$Decode$field,
						'_arg',
						A2(elm$json$Json$Decode$map, author$project$Models$Open, localParamA));
				default:
					var other = temp;
					return elm$json$Json$Decode$fail(other);
			}
		},
		A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
};
var author$project$Models$decodeExposed = function (localParamA) {
	return A2(
		elm$json$Json$Decode$andThen,
		function (localKeyFirst) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (localKeySubsequent) {
					return elm$json$Json$Decode$succeed(
						{first: localKeyFirst, subsequent: localKeySubsequent});
				},
				A2(
					elm$json$Json$Decode$field,
					'subsequent',
					author$project$Models$decodeList(
						author$project$Models$decodeClosedOr(localParamA))));
		},
		A2(elm$json$Json$Decode$field, 'first', localParamA));
};
var author$project$Models$Unit = {$: 'Unit'};
var author$project$Models$decodeUnit = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		if (temp === 'Unit') {
			return elm$json$Json$Decode$succeed(author$project$Models$Unit);
		} else {
			var other = temp;
			return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Models$decodeTable = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'Starting':
				return elm$json$Json$Decode$succeed(author$project$Models$Starting);
			case 'TrickDoubleBoth':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickDoubleBoth,
						author$project$Models$decodeExposed(author$project$Models$decodeBigWithDesign)));
			case 'TrickDoubleWen':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickDoubleWen,
						author$project$Models$decodeExposed(author$project$Models$decodeCardWen)));
			case 'TrickDoubleWu':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickDoubleWu,
						author$project$Models$decodeExposed(author$project$Models$decodeCardWuNumber)));
			case 'TrickQuadruple':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickQuadruple,
						author$project$Models$decodeExposed(author$project$Models$decodeCardBig)));
			case 'TrickSingleWen':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickSingleWen,
						author$project$Models$decodeExposed(author$project$Models$decodeCardWen)));
			case 'TrickSingleWu':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickSingleWu,
						author$project$Models$decodeExposed(author$project$Models$decodeCardWu)));
			case 'TrickTripleWen':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickTripleWen,
						author$project$Models$decodeExposed(author$project$Models$decodeBigWithDesign)));
			case 'TrickTripleWu':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickTripleWu,
						author$project$Models$decodeExposed(author$project$Models$decodeCardBig)));
			case 'TrickWenzun':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickWenzun,
						author$project$Models$decodeExposed(author$project$Models$decodeBool)));
			case 'TrickWuzun':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$TrickWuzun,
						author$project$Models$decodeExposed(author$project$Models$decodeUnit)));
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Models$decodeObservableInningState = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyGains) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyStartsAt) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeyTable) {
						return A2(
							elm$json$Json$Decode$andThen,
							function (localKeyYourHand) {
								return elm$json$Json$Decode$succeed(
									{gains: localKeyGains, startsAt: localKeyStartsAt, table: localKeyTable, yourHand: localKeyYourHand});
							},
							A2(
								elm$json$Json$Decode$field,
								'your_hand',
								author$project$Models$decodeList(author$project$Models$decodeCard)));
					},
					A2(elm$json$Json$Decode$field, 'table', author$project$Models$decodeTable));
			},
			A2(elm$json$Json$Decode$field, 'starts_at', author$project$Models$decodeSeat));
	},
	A2(
		elm$json$Json$Decode$field,
		'gains',
		author$project$Models$decodePerSeat(
			author$project$Models$decodeList(author$project$Models$decodeCard))));
var author$project$Models$decodeObservableInning = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'ObservableDuringInning':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$ObservableDuringInning, author$project$Models$decodeObservableInningState));
			case 'ObservableInningEnd':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$ObservableInningEnd,
						author$project$Models$decodePerSeat(
							author$project$Models$decodeList(author$project$Models$decodeCard))));
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Models$decodeSnapshotId = author$project$Models$decodeString;
var author$project$Models$decodeObservableGameState = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyMeta) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyObservableInning) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeySnapshotId) {
						return A2(
							elm$json$Json$Decode$andThen,
							function (localKeySynchronizing) {
								return elm$json$Json$Decode$succeed(
									{meta: localKeyMeta, observableInning: localKeyObservableInning, snapshotId: localKeySnapshotId, synchronizing: localKeySynchronizing});
							},
							A2(elm$json$Json$Decode$field, 'synchronizing', author$project$Models$decodeBool));
					},
					A2(elm$json$Json$Decode$field, 'snapshot_id', author$project$Models$decodeSnapshotId));
			},
			A2(elm$json$Json$Decode$field, 'observable_inning', author$project$Models$decodeObservableInning));
	},
	A2(elm$json$Json$Decode$field, 'meta', author$project$Models$decodeGameMeta));
var author$project$Models$decodeSubmission = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyNewState) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeySeat) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeySubmitted) {
						return A2(
							elm$json$Json$Decode$andThen,
							function (localKeyTrickLast) {
								return elm$json$Json$Decode$succeed(
									{newState: localKeyNewState, seat: localKeySeat, submitted: localKeySubmitted, trickLast: localKeyTrickLast});
							},
							A2(
								elm$json$Json$Decode$field,
								'trick_last',
								author$project$Models$decodeOption(author$project$Models$decodeTable)));
					},
					A2(
						elm$json$Json$Decode$field,
						'submitted',
						author$project$Models$decodeList(
							author$project$Models$decodeOption(author$project$Models$decodeCard))));
			},
			A2(elm$json$Json$Decode$field, 'seat', author$project$Models$decodeSeat));
	},
	A2(elm$json$Json$Decode$field, 'new_state', author$project$Models$decodeObservableGameState));
var author$project$Models$decodeNotification = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'NotifyComment':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$NotifyComment, author$project$Models$decodeComment));
			case 'NotifyEntered':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$NotifyEntered, author$project$Models$decodeUser));
			case 'NotifyExited':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$NotifyExited, author$project$Models$decodeUser));
			case 'NotifyGameStart':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$NotifyGameStart, author$project$Models$decodeObservableGameState));
			case 'NotifyNextStep':
				return elm$json$Json$Decode$succeed(author$project$Models$NotifyNextStep);
			case 'NotifySubmission':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$NotifySubmission, author$project$Models$decodeSubmission));
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Port$receiveWebSocketMessage = _Platform_incomingPort('receiveWebSocketMessage', elm$json$Json$Decode$string);
var author$project$WebSocketClient$subscribe = author$project$Port$receiveWebSocketMessage(
	function (s) {
		return author$project$Common$ReceiveNotification(
			A2(elm$json$Json$Decode$decodeString, author$project$Models$decodeNotification, s));
	});
var elm$browser$Browser$Events$Window = {$: 'Window'};
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$core$Process$kill = _Scheduler_kill;
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.key;
		var event = _n0.event;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		elm$browser$Browser$Events$on,
		elm$browser$Browser$Events$Window,
		'resize',
		A2(
			elm$json$Json$Decode$field,
			'target',
			A3(
				elm$json$Json$Decode$map2,
				func,
				A2(elm$json$Json$Decode$field, 'innerWidth', elm$json$Json$Decode$int),
				A2(elm$json$Json$Decode$field, 'innerHeight', elm$json$Json$Decode$int))));
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var elm$time$Time$init = elm$core$Task$succeed(
	A2(elm$time$Time$State, elm$core$Dict$empty, elm$core$Dict$empty));
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$time$Time$addMySub = F2(
	function (_n0, state) {
		var interval = _n0.a;
		var tagger = _n0.b;
		var _n1 = A2(elm$core$Dict$get, interval, state);
		if (_n1.$ === 'Nothing') {
			return A3(
				elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _n1.a;
			return A3(
				elm$core$Dict$insert,
				interval,
				A2(elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var elm$time$Time$customZone = elm$time$Time$Zone;
var elm$time$Time$setInterval = _Time_setInterval;
var elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = elm$core$Process$spawn(
				A2(
					elm$time$Time$setInterval,
					interval,
					A2(elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					elm$time$Time$spawnHelp,
					router,
					rest,
					A3(elm$core$Dict$insert, interval, id, processes));
			};
			return A2(elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var elm$time$Time$onEffects = F3(
	function (router, subs, _n0) {
		var processes = _n0.processes;
		var rightStep = F3(
			function (_n6, id, _n7) {
				var spawns = _n7.a;
				var existing = _n7.b;
				var kills = _n7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						elm$core$Task$andThen,
						function (_n5) {
							return kills;
						},
						elm$core$Process$kill(id)));
			});
		var newTaggers = A3(elm$core$List$foldl, elm$time$Time$addMySub, elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _n4) {
				var spawns = _n4.a;
				var existing = _n4.b;
				var kills = _n4.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _n3) {
				var spawns = _n3.a;
				var existing = _n3.b;
				var kills = _n3.c;
				return _Utils_Tuple3(
					spawns,
					A3(elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _n1 = A6(
			elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				elm$core$Dict$empty,
				elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _n1.a;
		var existingDict = _n1.b;
		var killTask = _n1.c;
		return A2(
			elm$core$Task$andThen,
			function (newProcesses) {
				return elm$core$Task$succeed(
					A2(elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				elm$core$Task$andThen,
				function (_n2) {
					return A3(elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var elm$time$Time$now = _Time_now(elm$time$Time$millisToPosix);
var elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _n0 = A2(elm$core$Dict$get, interval, state.taggers);
		if (_n0.$ === 'Nothing') {
			return elm$core$Task$succeed(state);
		} else {
			var taggers = _n0.a;
			var tellTaggers = function (time) {
				return elm$core$Task$sequence(
					A2(
						elm$core$List$map,
						function (tagger) {
							return A2(
								elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$succeed(state);
				},
				A2(elm$core$Task$andThen, tellTaggers, elm$time$Time$now));
		}
	});
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$time$Time$subMap = F2(
	function (f, _n0) {
		var interval = _n0.a;
		var tagger = _n0.b;
		return A2(
			elm$time$Time$Every,
			interval,
			A2(elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager(elm$time$Time$init, elm$time$Time$onEffects, elm$time$Time$onSelfMsg, 0, elm$time$Time$subMap);
var elm$time$Time$subscription = _Platform_leaf('Time');
var elm$time$Time$every = F2(
	function (interval, tagger) {
		return elm$time$Time$subscription(
			A2(elm$time$Time$Every, interval, tagger));
	});
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				author$project$WebSocketClient$onOpen,
				author$project$WebSocketClient$subscribe,
				A2(
				elm$time$Time$every,
				author$project$Constants$heartbeatIntervalMs,
				function (_n0) {
					return author$project$Common$Heartbeat;
				}),
				elm$browser$Browser$Events$onResize(author$project$Common$WindowResized)
			]));
};
var author$project$Common$AtPlaza = F4(
	function (a, b, c, d) {
		return {$: 'AtPlaza', a: a, b: b, c: c, d: d};
	});
var author$project$Common$InRoom = F5(
	function (a, b, c, d, e) {
		return {$: 'InRoom', a: a, b: b, c: c, d: d, e: e};
	});
var author$project$Common$TransitionToNextTrick = function (a) {
	return {$: 'TransitionToNextTrick', a: a};
};
var author$project$Common$Warning = {$: 'Warning'};
var elm$core$Process$sleep = _Process_sleep;
var author$project$Common$sendAfter = F2(
	function (time, msg) {
		return A2(
			elm$core$Task$perform,
			function (msg0) {
				return msg0;
			},
			A2(
				elm$core$Task$andThen,
				function (_n0) {
					return elm$core$Task$succeed(msg);
				},
				elm$core$Process$sleep(time)));
	});
var author$project$Constants$trickLastTimeMs = 1500.0;
var author$project$Common$ReceiveResponse = function (a) {
	return {$: 'ReceiveResponse', a: a};
};
var author$project$Common$RoomCreated = F2(
	function (a, b) {
		return {$: 'RoomCreated', a: a, b: b};
	});
var author$project$Models$decodeCreateRoomResponse = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyRoomId) {
		return elm$json$Json$Decode$succeed(
			{roomId: localKeyRoomId});
	},
	A2(elm$json$Json$Decode$field, 'room_id', author$project$Models$decodeRoomId));
var author$project$Models$encodeString = elm$json$Json$Encode$string;
var author$project$Models$encodeRoomName = author$project$Models$encodeString;
var author$project$Models$encodeUserId = author$project$Models$encodeString;
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var author$project$Models$encodeCreateRoomRequest = function (localParamTemp) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'room_name',
				author$project$Models$encodeRoomName(localParamTemp.roomName)),
				_Utils_Tuple2(
				'user_id',
				author$project$Models$encodeUserId(localParamTemp.userId))
			]));
};
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var elm$http$Http$Timeout_ = {$: 'Timeout_'};
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var elm$http$Http$NetworkError = {$: 'NetworkError'};
var elm$http$Http$Timeout = {$: 'Timeout'};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 'NetworkError_':
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			elm$http$Http$expectStringResponse,
			toMsg,
			elm$http$Http$resolve(
				function (string) {
					return A2(
						elm$core$Result$mapError,
						elm$json$Json$Decode$errorToString,
						A2(elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2(elm$json$Json$Encode$encode, 0, value));
};
var elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.tracker;
							if (_n4.$ === 'Nothing') {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var elm$http$Http$post = function (r) {
	return elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: elm$core$Maybe$Nothing, tracker: elm$core$Maybe$Nothing, url: r.url});
};
var author$project$HttpClient$createRoom = F3(
	function (origin, userId, roomName) {
		return elm$http$Http$post(
			{
				body: elm$http$Http$jsonBody(
					author$project$Models$encodeCreateRoomRequest(
						{roomName: roomName, userId: userId})),
				expect: A2(
					elm$http$Http$expectJson,
					A2(
						elm$core$Basics$composeL,
						author$project$Common$ReceiveResponse,
						author$project$Common$RoomCreated(roomName)),
					author$project$Models$decodeCreateRoomResponse),
				url: origin + '/rooms'
			});
	});
var author$project$Common$UserCreated = F2(
	function (a, b) {
		return {$: 'UserCreated', a: a, b: b};
	});
var author$project$Models$decodeCreateUserResponse = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyUserId) {
		return elm$json$Json$Decode$succeed(
			{userId: localKeyUserId});
	},
	A2(elm$json$Json$Decode$field, 'user_id', author$project$Models$decodeUserId));
var author$project$Models$encodeUserName = author$project$Models$encodeString;
var author$project$Models$encodeCreateUserRequest = function (localParamTemp) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'user_name',
				author$project$Models$encodeUserName(localParamTemp.userName))
			]));
};
var author$project$HttpClient$createUser = F2(
	function (origin, userName) {
		return elm$http$Http$post(
			{
				body: elm$http$Http$jsonBody(
					author$project$Models$encodeCreateUserRequest(
						{userName: userName})),
				expect: A2(
					elm$http$Http$expectJson,
					A2(
						elm$core$Basics$composeL,
						author$project$Common$ReceiveResponse,
						author$project$Common$UserCreated(userName)),
					author$project$Models$decodeCreateUserResponse),
				url: origin + '/users'
			});
	});
var author$project$Common$RoomEntered = F2(
	function (a, b) {
		return {$: 'RoomEntered', a: a, b: b};
	});
var author$project$Models$RoomRequestToEnterRoom = function (a) {
	return {$: 'RoomRequestToEnterRoom', a: a};
};
var author$project$Models$LogComment = function (a) {
	return {$: 'LogComment', a: a};
};
var author$project$Models$LogEntered = function (a) {
	return {$: 'LogEntered', a: a};
};
var author$project$Models$LogExited = function (a) {
	return {$: 'LogExited', a: a};
};
var author$project$Models$LogGameStart = {$: 'LogGameStart'};
var author$project$Models$decodeLog = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'LogComment':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$LogComment, author$project$Models$decodeComment));
			case 'LogEntered':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$LogEntered, author$project$Models$decodeUser));
			case 'LogExited':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$LogExited, author$project$Models$decodeUser));
			case 'LogGameStart':
				return elm$json$Json$Decode$succeed(author$project$Models$LogGameStart);
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Models$PlayingGame = function (a) {
	return {$: 'PlayingGame', a: a};
};
var author$project$Models$WaitingStart = function (a) {
	return {$: 'WaitingStart', a: a};
};
var author$project$Models$decodePersonalRoomState = A2(
	elm$json$Json$Decode$andThen,
	function (temp) {
		switch (temp) {
			case 'PlayingGame':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(elm$json$Json$Decode$map, author$project$Models$PlayingGame, author$project$Models$decodeObservableGameState));
			case 'WaitingStart':
				return A2(
					elm$json$Json$Decode$field,
					'_arg',
					A2(
						elm$json$Json$Decode$map,
						author$project$Models$WaitingStart,
						author$project$Models$decodeList(author$project$Models$decodeUser)));
			default:
				var other = temp;
				return elm$json$Json$Decode$fail(other);
		}
	},
	A2(elm$json$Json$Decode$field, '_label', elm$json$Json$Decode$string));
var author$project$Models$decodeRoomName = author$project$Models$decodeString;
var author$project$Models$decodeRoom = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyRoomId) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyRoomName) {
				return elm$json$Json$Decode$succeed(
					{roomId: localKeyRoomId, roomName: localKeyRoomName});
			},
			A2(elm$json$Json$Decode$field, 'room_name', author$project$Models$decodeRoomName));
	},
	A2(elm$json$Json$Decode$field, 'room_id', author$project$Models$decodeRoomId));
var author$project$Models$decodePersonalState = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyGame) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyLogs) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeyRoom) {
						return elm$json$Json$Decode$succeed(
							{game: localKeyGame, logs: localKeyLogs, room: localKeyRoom});
					},
					A2(elm$json$Json$Decode$field, 'room', author$project$Models$decodeRoom));
			},
			A2(
				elm$json$Json$Decode$field,
				'logs',
				author$project$Models$decodeList(author$project$Models$decodeLog)));
	},
	A2(elm$json$Json$Decode$field, 'game', author$project$Models$decodePersonalRoomState));
var author$project$Models$decodeEnterRoomResponse = author$project$Models$decodePersonalState;
var author$project$Models$encodeEnterRoomRequest = function (localParamTemp) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'user_id',
				author$project$Models$encodeUserId(localParamTemp.userId))
			]));
};
var elm$json$Json$Encode$int = _Json_wrap;
var author$project$Models$encodeInt = elm$json$Json$Encode$int;
var author$project$Models$encodeCardWen = author$project$Models$encodeInt;
var author$project$Models$encodeCardWuNumber = author$project$Models$encodeInt;
var elm$json$Json$Encode$bool = _Json_wrap;
var author$project$Models$encodeBool = elm$json$Json$Encode$bool;
var author$project$Models$encodeDesign = author$project$Models$encodeBool;
var author$project$Models$encodeCardWu = function (localParamTemp) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'design',
				author$project$Models$encodeDesign(localParamTemp.design)),
				_Utils_Tuple2(
				'number',
				author$project$Models$encodeCardWuNumber(localParamTemp.number))
			]));
};
var author$project$Models$encodeCard = function (temp) {
	if (temp.$ === 'Wen') {
		var sub = temp.a;
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'_label',
					elm$json$Json$Encode$string('Wen')),
					_Utils_Tuple2(
					'_arg',
					author$project$Models$encodeCardWen(sub))
				]));
	} else {
		var sub = temp.a;
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'_label',
					elm$json$Json$Encode$string('Wu')),
					_Utils_Tuple2(
					'_arg',
					author$project$Models$encodeCardWu(sub))
				]));
	}
};
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var author$project$Models$encodeList = elm$json$Json$Encode$list;
var author$project$Models$encodeSubmitCardsRequest = function (localParamTemp) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'cards',
				author$project$Models$encodeList(author$project$Models$encodeCard)(localParamTemp.cards)),
				_Utils_Tuple2(
				'user_id',
				author$project$Models$encodeUserId(localParamTemp.userId))
			]));
};
var author$project$Models$encodeRoomRequest = function (temp) {
	if (temp.$ === 'RoomRequestToEnterRoom') {
		var sub = temp.a;
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'_label',
					elm$json$Json$Encode$string('RoomRequestToEnterRoom')),
					_Utils_Tuple2(
					'_arg',
					author$project$Models$encodeEnterRoomRequest(sub))
				]));
	} else {
		var sub = temp.a;
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'_label',
					elm$json$Json$Encode$string('RoomRequestToSubmitCards')),
					_Utils_Tuple2(
					'_arg',
					author$project$Models$encodeSubmitCardsRequest(sub))
				]));
	}
};
var author$project$HttpClient$enterRoom = F3(
	function (origin, userId, roomId) {
		return elm$http$Http$request(
			{
				body: elm$http$Http$jsonBody(
					author$project$Models$encodeRoomRequest(
						author$project$Models$RoomRequestToEnterRoom(
							{userId: userId}))),
				expect: A2(
					elm$http$Http$expectJson,
					A2(
						elm$core$Basics$composeL,
						author$project$Common$ReceiveResponse,
						author$project$Common$RoomEntered(roomId)),
					author$project$Models$decodeEnterRoomResponse),
				headers: _List_Nil,
				method: 'PATCH',
				timeout: elm$core$Maybe$Nothing,
				tracker: elm$core$Maybe$Nothing,
				url: origin + ('/rooms/' + roomId)
			});
	});
var author$project$Common$AllRoomsGot = function (a) {
	return {$: 'AllRoomsGot', a: a};
};
var author$project$Models$decodeRoomSummary = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyIsPlaying) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyMembers) {
				return A2(
					elm$json$Json$Decode$andThen,
					function (localKeyRoom) {
						return elm$json$Json$Decode$succeed(
							{isPlaying: localKeyIsPlaying, members: localKeyMembers, room: localKeyRoom});
					},
					A2(elm$json$Json$Decode$field, 'room', author$project$Models$decodeRoom));
			},
			A2(
				elm$json$Json$Decode$field,
				'members',
				author$project$Models$decodeList(author$project$Models$decodeUser)));
	},
	A2(elm$json$Json$Decode$field, 'is_playing', author$project$Models$decodeBool));
var author$project$Models$decodeGetAllRoomsResponse = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyRooms) {
		return elm$json$Json$Decode$succeed(
			{rooms: localKeyRooms});
	},
	A2(
		elm$json$Json$Decode$field,
		'rooms',
		author$project$Models$decodeList(author$project$Models$decodeRoomSummary)));
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$http$Http$get = function (r) {
	return elm$http$Http$request(
		{body: elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: elm$core$Maybe$Nothing, tracker: elm$core$Maybe$Nothing, url: r.url});
};
var author$project$HttpClient$getAllRooms = function (origin) {
	return elm$http$Http$get(
		{
			expect: A2(
				elm$http$Http$expectJson,
				A2(elm$core$Basics$composeL, author$project$Common$ReceiveResponse, author$project$Common$AllRoomsGot),
				author$project$Models$decodeGetAllRoomsResponse),
			url: origin + '/rooms'
		});
};
var author$project$Common$RoomGot = F2(
	function (a, b) {
		return {$: 'RoomGot', a: a, b: b};
	});
var author$project$Models$decodeGetRoomResponse = author$project$Models$decodePersonalState;
var author$project$HttpClient$getRoom = F3(
	function (origin, userId, roomId) {
		return elm$http$Http$get(
			{
				expect: A2(
					elm$http$Http$expectJson,
					A2(
						elm$core$Basics$composeL,
						author$project$Common$ReceiveResponse,
						author$project$Common$RoomGot(roomId)),
					author$project$Models$decodeGetRoomResponse),
				url: origin + ('/rooms/' + (roomId + ('/users/' + userId)))
			});
	});
var author$project$Common$SubmissionDone = function (a) {
	return {$: 'SubmissionDone', a: a};
};
var author$project$Models$RoomRequestToSubmitCards = function (a) {
	return {$: 'RoomRequestToSubmitCards', a: a};
};
var author$project$Models$decodeSubmitCardsResponse = A2(
	elm$json$Json$Decode$andThen,
	function (localKeyNewState) {
		return A2(
			elm$json$Json$Decode$andThen,
			function (localKeyTrickLast) {
				return elm$json$Json$Decode$succeed(
					{newState: localKeyNewState, trickLast: localKeyTrickLast});
			},
			A2(
				elm$json$Json$Decode$field,
				'trick_last',
				author$project$Models$decodeOption(author$project$Models$decodeTable)));
	},
	A2(elm$json$Json$Decode$field, 'new_state', author$project$Models$decodeObservableGameState));
var author$project$HttpClient$submitCards = F4(
	function (origin, userId, roomId, cards) {
		return elm$http$Http$request(
			{
				body: elm$http$Http$jsonBody(
					author$project$Models$encodeRoomRequest(
						author$project$Models$RoomRequestToSubmitCards(
							{cards: cards, userId: userId}))),
				expect: A2(
					elm$http$Http$expectJson,
					A2(elm$core$Basics$composeL, author$project$Common$ReceiveResponse, author$project$Common$SubmissionDone),
					author$project$Models$decodeSubmitCardsResponse),
				headers: _List_Nil,
				method: 'PATCH',
				timeout: elm$core$Maybe$Nothing,
				tracker: elm$core$Maybe$Nothing,
				url: origin + ('/rooms/' + roomId)
			});
	});
var author$project$Main$showHttpError = function (err) {
	switch (err.$) {
		case 'BadUrl':
			var s = err.a;
			return 'bad URL; ' + s;
		case 'Timeout':
			return 'request timeout';
		case 'BadBody':
			var s = err.a;
			return 'bad body; ' + s;
		case 'NetworkError':
			return 'network error';
		default:
			var n = err.a;
			return 'bad status; ' + elm$core$String$fromInt(n);
	}
};
var author$project$Main$makeErrorMessage = F2(
	function (category, err) {
		var msg = author$project$Main$showHttpError(err);
		return _Utils_Tuple2(author$project$Common$Warning, '[error] ' + (category + (': ' + msg)));
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var author$project$Main$pickupSelectedCards = F2(
	function (indices, cards) {
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n0, cardAcc) {
					var index = _n0.a;
					var card = _n0.b;
					return A2(elm$core$Set$member, index, indices) ? A2(elm$core$List$cons, card, cardAcc) : cardAcc;
				}),
			_List_Nil,
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, card) {
						return _Utils_Tuple2(index, card);
					}),
				cards));
	});
var author$project$Main$showNotification = function (notification) {
	switch (notification.$) {
		case 'NotifyComment':
			return 'NotifyComment';
		case 'NotifyEntered':
			return 'NotifyEntered';
		case 'NotifyExited':
			return 'NotifyExited';
		case 'NotifyGameStart':
			return 'NotifyGameStart';
		case 'NotifyNextStep':
			return 'NotifyNextStep';
		default:
			return 'NotifySubmission';
	}
};
var author$project$Main$showResponse = function (resp) {
	if (resp.$ === 'SubmissionDone') {
		if (resp.a.$ === 'Err') {
			var err = resp.a.a;
			return 'SubmissionDone (error: ' + (author$project$Main$showHttpError(err) + ')');
		} else {
			return 'SubmissionDone (ok)';
		}
	} else {
		return '<response>';
	}
};
var author$project$Main$showMessage = function (msg) {
	switch (msg.$) {
		case 'UpdateInput':
			return 'UpdateInput';
		case 'SendRequest':
			return 'SendRequest';
		case 'ReceiveResponse':
			var resp = msg.a;
			return 'ReceiveResponse (' + (author$project$Main$showResponse(resp) + ')');
		case 'ReceiveNotification':
			if (msg.a.$ === 'Err') {
				return 'ReceiveNotification (error)';
			} else {
				var nt = msg.a.a;
				return 'ReceiveNotification (' + (author$project$Main$showNotification(nt) + ')');
			}
		case 'OpenWebSocket':
			return 'OpenWebSocket';
		case 'SelectCard':
			return 'SelectCard';
		case 'UnselectCard':
			return 'UnselectCard';
		case 'TransitionToNextTrick':
			return 'TransitionToNextTrick';
		case 'Heartbeat':
			return 'Heartbeat';
		default:
			return 'WindowResized';
	}
};
var author$project$Models$CommandNextInning = function (a) {
	return {$: 'CommandNextInning', a: a};
};
var author$project$Models$encodeSnapshotId = author$project$Models$encodeString;
var author$project$Models$encodeCommand = function (temp) {
	switch (temp.$) {
		case 'CommandAck':
			var sub = temp.a;
			return elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'_label',
						elm$json$Json$Encode$string('CommandAck')),
						_Utils_Tuple2(
						'_arg',
						author$project$Models$encodeSnapshotId(sub))
					]));
		case 'CommandComment':
			var sub = temp.a;
			return elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'_label',
						elm$json$Json$Encode$string('CommandComment')),
						_Utils_Tuple2(
						'_arg',
						author$project$Models$encodeString(sub))
					]));
		case 'CommandHeartbeat':
			return elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'_label',
						elm$json$Json$Encode$string('CommandHeartbeat'))
					]));
		default:
			var sub = temp.a;
			return elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'_label',
						elm$json$Json$Encode$string('CommandNextInning')),
						_Utils_Tuple2(
						'_arg',
						author$project$Models$encodeSnapshotId(sub))
					]));
	}
};
var author$project$Port$sendWebSocketMessage = _Platform_outgoingPort(
	'sendWebSocketMessage',
	function ($) {
		var a = $.a;
		var b = $.b;
		return A2(
			elm$json$Json$Encode$list,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					elm$core$Basics$identity(a),
					elm$json$Json$Encode$string(b)
				]));
	});
var author$project$WebSocketClient$requireNextInning = F2(
	function (ws, snapshotId) {
		var s = A2(
			elm$json$Json$Encode$encode,
			0,
			author$project$Models$encodeCommand(
				author$project$Models$CommandNextInning(snapshotId)));
		return author$project$Port$sendWebSocketMessage(
			_Utils_Tuple2(ws, s));
	});
var author$project$Models$CommandAck = function (a) {
	return {$: 'CommandAck', a: a};
};
var author$project$WebSocketClient$sendAck = F2(
	function (ws, snapshotId) {
		var s = A2(
			elm$json$Json$Encode$encode,
			0,
			author$project$Models$encodeCommand(
				author$project$Models$CommandAck(snapshotId)));
		return author$project$Port$sendWebSocketMessage(
			_Utils_Tuple2(ws, s));
	});
var author$project$Models$CommandComment = function (a) {
	return {$: 'CommandComment', a: a};
};
var author$project$WebSocketClient$sendChat = F2(
	function (ws, text) {
		var s = A2(
			elm$json$Json$Encode$encode,
			0,
			author$project$Models$encodeCommand(
				author$project$Models$CommandComment(text)));
		return author$project$Port$sendWebSocketMessage(
			_Utils_Tuple2(ws, s));
	});
var author$project$Models$CommandHeartbeat = {$: 'CommandHeartbeat'};
var author$project$WebSocketClient$sendHeartbeat = function (ws) {
	var s = A2(
		elm$json$Json$Encode$encode,
		0,
		author$project$Models$encodeCommand(author$project$Models$CommandHeartbeat));
	return author$project$Port$sendWebSocketMessage(
		_Utils_Tuple2(ws, s));
};
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Set$remove = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A2(elm$core$Dict$remove, key, dict));
	});
var author$project$Main$update = F2(
	function (msg, model) {
		var _n0 = model.state;
		switch (_n0.$) {
			case 'AtEntrance':
				var userNameInput = _n0.a;
				var maybeUserAndRoom = _n0.b;
				_n1$6:
				while (true) {
					switch (msg.$) {
						case 'WindowResized':
							var width = msg.a;
							var height = msg.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										window: {height: height, width: width}
									}),
								elm$core$Platform$Cmd$none);
						case 'Heartbeat':
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						case 'UpdateInput':
							if (msg.a.$ === 'UserNameInput') {
								var userNameInput1 = msg.a.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											state: A2(author$project$Common$AtEntrance, userNameInput1, maybeUserAndRoom)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								break _n1$6;
							}
						case 'SendRequest':
							if (msg.a.$ === 'CreateUser') {
								var _n2 = msg.a;
								var cmd = A2(author$project$HttpClient$createUser, model.origin, userNameInput);
								return _Utils_Tuple2(model, cmd);
							} else {
								break _n1$6;
							}
						case 'ReceiveResponse':
							if (msg.a.$ === 'UserCreated') {
								var _n3 = msg.a;
								var userName = _n3.a;
								var result = _n3.b;
								if (result.$ === 'Ok') {
									var responseBody = result.a;
									var userId = responseBody.userId;
									var user = {userId: userId, userName: userName};
									var wsOrigin = author$project$Main$makeWebsocketOrigin(model.origin);
									var cmd = A2(author$project$WebSocketClient$listen, wsOrigin, userId);
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												state: A2(
													author$project$Common$AtEntrance,
													userNameInput,
													elm$core$Maybe$Just(
														_Utils_Tuple2(user, elm$core$Maybe$Nothing)))
											}),
										cmd);
								} else {
									var err = result.a;
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												message: A2(author$project$Main$makeErrorMessage, 'user creation', err)
											}),
										elm$core$Platform$Cmd$none);
								}
							} else {
								break _n1$6;
							}
						case 'OpenWebSocket':
							var ws = msg.a;
							if (maybeUserAndRoom.$ === 'Just') {
								if (maybeUserAndRoom.a.b.$ === 'Nothing') {
									var _n6 = maybeUserAndRoom.a;
									var user = _n6.a;
									var _n7 = _n6.b;
									var cmd = author$project$HttpClient$getAllRooms(model.origin);
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												state: A4(author$project$Common$AtPlaza, ws, user, '', elm$core$Maybe$Nothing)
											}),
										cmd);
								} else {
									var _n8 = maybeUserAndRoom.a;
									var user = _n8.a;
									var roomId = _n8.b.a;
									var cmd = A3(author$project$HttpClient$getRoom, model.origin, user.userId, roomId);
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												state: A4(author$project$Common$AtPlaza, ws, user, '', elm$core$Maybe$Nothing)
											}),
										cmd);
								}
							} else {
								var message = _Utils_Tuple2(
									author$project$Common$Warning,
									'unexpected message (AtEntrance): ' + author$project$Main$showMessage(msg));
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{message: message}),
									elm$core$Platform$Cmd$none);
							}
						default:
							break _n1$6;
					}
				}
				var message = _Utils_Tuple2(
					author$project$Common$Warning,
					'unexpected message (AtEntrance): ' + author$project$Main$showMessage(msg));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{message: message}),
					elm$core$Platform$Cmd$none);
			case 'AtPlaza':
				var ws = _n0.a;
				var user = _n0.b;
				var roomNameInput0 = _n0.c;
				var maybeRooms = _n0.d;
				_n9$9:
				while (true) {
					switch (msg.$) {
						case 'WindowResized':
							var width = msg.a;
							var height = msg.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										window: {height: height, width: width}
									}),
								elm$core$Platform$Cmd$none);
						case 'Heartbeat':
							var cmd = author$project$WebSocketClient$sendHeartbeat(ws);
							return _Utils_Tuple2(model, cmd);
						case 'UpdateInput':
							if (msg.a.$ === 'RoomNameInput') {
								var roomNameInput1 = msg.a.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											state: A4(author$project$Common$AtPlaza, ws, user, roomNameInput1, maybeRooms)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								break _n9$9;
							}
						case 'SendRequest':
							switch (msg.a.$) {
								case 'CreateRoom':
									var _n11 = msg.a;
									var cmd = A3(author$project$HttpClient$createRoom, model.origin, user.userId, roomNameInput0);
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												state: A4(author$project$Common$AtPlaza, ws, user, '', maybeRooms)
											}),
										cmd);
								case 'EnterRoom':
									var roomId = msg.a.a;
									var cmd = A3(author$project$HttpClient$enterRoom, model.origin, user.userId, roomId);
									return _Utils_Tuple2(model, cmd);
								default:
									break _n9$9;
							}
						case 'ReceiveResponse':
							switch (msg.a.$) {
								case 'AllRoomsGot':
									var result = msg.a.a;
									if (result.$ === 'Ok') {
										var responseBody = result.a;
										var rooms = responseBody.rooms;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													state: A4(
														author$project$Common$AtPlaza,
														ws,
														user,
														roomNameInput0,
														elm$core$Maybe$Just(rooms))
												}),
											elm$core$Platform$Cmd$none);
									} else {
										var err = result.a;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													message: A2(author$project$Main$makeErrorMessage, 'rooms', err)
												}),
											elm$core$Platform$Cmd$none);
									}
								case 'RoomCreated':
									var _n12 = msg.a;
									var roomName = _n12.a;
									var result = _n12.b;
									if (result.$ === 'Ok') {
										var responseBody = result.a;
										if (maybeRooms.$ === 'Just') {
											var roomSummaries0 = maybeRooms.a;
											var roomId = responseBody.roomId;
											var roomSummary = {
												isPlaying: false,
												members: _List_Nil,
												room: {roomId: roomId, roomName: roomName}
											};
											var roomSummaries1 = _Utils_ap(
												roomSummaries0,
												_List_fromArray(
													[roomSummary]));
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{
														state: A4(
															author$project$Common$AtPlaza,
															ws,
															user,
															roomNameInput0,
															elm$core$Maybe$Just(roomSummaries1))
													}),
												elm$core$Platform$Cmd$none);
										} else {
											return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
										}
									} else {
										var err = result.a;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													message: A2(author$project$Main$makeErrorMessage, 'room creation', err)
												}),
											elm$core$Platform$Cmd$none);
									}
								case 'RoomEntered':
									var _n15 = msg.a;
									var roomId = _n15.a;
									var result = _n15.b;
									if (result.$ === 'Ok') {
										var pstate0 = result.a;
										var _n17 = pstate0.game;
										if (_n17.$ === 'WaitingStart') {
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{
														state: A5(author$project$Common$InRoom, ws, user, pstate0, elm$core$Set$empty, '')
													}),
												elm$core$Platform$Cmd$none);
										} else {
											var ostate0 = _n17.a;
											var cmd = A2(author$project$WebSocketClient$sendAck, ws, ostate0.snapshotId);
											var ostate1 = _Utils_update(
												ostate0,
												{synchronizing: true});
											var pstate1 = _Utils_update(
												pstate0,
												{
													game: author$project$Models$PlayingGame(ostate1)
												});
											return A2(
												elm$core$Debug$log,
												'RoomEntered (+)',
												_Utils_Tuple2(
													_Utils_update(
														model,
														{
															state: A5(author$project$Common$InRoom, ws, user, pstate1, elm$core$Set$empty, '')
														}),
													cmd));
										}
									} else {
										var err = result.a;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													message: A2(author$project$Main$makeErrorMessage, 'enter room', err)
												}),
											elm$core$Platform$Cmd$none);
									}
								case 'RoomGot':
									var _n18 = msg.a;
									var roomId = _n18.a;
									var result = _n18.b;
									if (result.$ === 'Ok') {
										var pstate = result.a;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													state: A5(author$project$Common$InRoom, ws, user, pstate, elm$core$Set$empty, '')
												}),
											elm$core$Platform$Cmd$none);
									} else {
										var err = result.a;
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													message: A2(author$project$Main$makeErrorMessage, 'got room', err)
												}),
											elm$core$Platform$Cmd$none);
									}
								default:
									break _n9$9;
							}
						default:
							break _n9$9;
					}
				}
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							message: _Utils_Tuple2(
								author$project$Common$Warning,
								'unexpected message (AtPlaza): ' + author$project$Main$showMessage(msg))
						}),
					elm$core$Platform$Cmd$none);
			default:
				var ws = _n0.a;
				var user = _n0.b;
				var pstate0 = _n0.c;
				var indices0 = _n0.d;
				var chatTextInput0 = _n0.e;
				var _n20 = _Utils_Tuple2(pstate0.game, msg);
				_n20$17:
				while (true) {
					switch (_n20.b.$) {
						case 'WindowResized':
							var _n21 = _n20.b;
							var width = _n21.a;
							var height = _n21.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										message: _Utils_Tuple2(
											author$project$Common$Information,
											'window resized. width: ' + elm$core$String$fromInt(width)),
										window: {height: height, width: width}
									}),
								elm$core$Platform$Cmd$none);
						case 'Heartbeat':
							var _n22 = _n20.b;
							var cmd = author$project$WebSocketClient$sendHeartbeat(ws);
							return _Utils_Tuple2(model, cmd);
						case 'UpdateInput':
							if (_n20.b.a.$ === 'ChatInput') {
								var chatTextInput1 = _n20.b.a.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											state: A5(author$project$Common$InRoom, ws, user, pstate0, indices0, chatTextInput1)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								break _n20$17;
							}
						case 'ReceiveResponse':
							if ((_n20.a.$ === 'PlayingGame') && (_n20.b.a.$ === 'SubmissionDone')) {
								var ostate0 = _n20.a.a;
								var res = _n20.b.a.a;
								var nextResult = function () {
									if (res.$ === 'Ok') {
										var submissionResponse = res.a;
										var newState = submissionResponse.newState;
										var _n29 = _Utils_Tuple2(submissionResponse.trickLast, ostate0.observableInning);
										if (_n29.a.$ === 'Just') {
											if (_n29.b.$ === 'ObservableDuringInning') {
												var lastTable = _n29.a.a;
												var oinning0 = _n29.b.a;
												var ostate1 = _Utils_update(
													ostate0,
													{
														observableInning: author$project$Models$ObservableDuringInning(
															_Utils_update(
																oinning0,
																{table: lastTable}))
													});
												var state1 = A5(
													author$project$Common$InRoom,
													ws,
													user,
													_Utils_update(
														pstate0,
														{
															game: author$project$Models$PlayingGame(ostate1)
														}),
													indices0,
													chatTextInput0);
												var cmd = A2(
													author$project$Common$sendAfter,
													author$project$Constants$trickLastTimeMs,
													author$project$Common$TransitionToNextTrick(newState));
												var message = ostate0.synchronizing ? model.message : _Utils_Tuple2(
													author$project$Common$Warning,
													'not synchronizing: ' + author$project$Main$showMessage(msg));
												return elm$core$Result$Ok(
													_Utils_Tuple2(
														_Utils_update(
															model,
															{message: message, state: state1}),
														cmd));
											} else {
												return elm$core$Result$Err(
													_Utils_Tuple2(
														author$project$Common$Warning,
														'unexpected message (Just, _): ' + author$project$Main$showMessage(msg)));
											}
										} else {
											var _n30 = _n29.a;
											var ostate1 = newState;
											var state1 = A5(
												author$project$Common$InRoom,
												ws,
												user,
												_Utils_update(
													pstate0,
													{
														game: author$project$Models$PlayingGame(ostate1)
													}),
												indices0,
												chatTextInput0);
											var cmd = A2(author$project$WebSocketClient$sendAck, ws, ostate1.snapshotId);
											var message = ostate0.synchronizing ? model.message : _Utils_Tuple2(
												author$project$Common$Warning,
												'not synchronizing: ' + author$project$Main$showMessage(msg));
											return elm$core$Result$Ok(
												_Utils_Tuple2(
													_Utils_update(
														model,
														{message: message, state: state1}),
													cmd));
										}
									} else {
										var err = res.a;
										return elm$core$Result$Err(
											A2(author$project$Main$makeErrorMessage, 'submission', err));
									}
								}();
								if (nextResult.$ === 'Ok') {
									var next = nextResult.a;
									return A2(elm$core$Debug$log, 'SubmissionDone', next);
								} else {
									var message = nextResult.a;
									var ostate1 = _Utils_update(
										ostate0,
										{synchronizing: false});
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												message: message,
												state: A5(
													author$project$Common$InRoom,
													ws,
													user,
													_Utils_update(
														pstate0,
														{
															game: author$project$Models$PlayingGame(ostate1)
														}),
													indices0,
													chatTextInput0)
											}),
										elm$core$Platform$Cmd$none);
								}
							} else {
								break _n20$17;
							}
						case 'TransitionToNextTrick':
							if (_n20.a.$ === 'PlayingGame') {
								var ostate0 = _n20.a.a;
								var ostate1 = _n20.b.a;
								if (ostate0.synchronizing) {
									var ostate2 = _Utils_update(
										ostate1,
										{synchronizing: true});
									var state1 = A5(
										author$project$Common$InRoom,
										ws,
										user,
										_Utils_update(
											pstate0,
											{
												game: author$project$Models$PlayingGame(ostate2)
											}),
										indices0,
										chatTextInput0);
									var cmd = A2(author$project$WebSocketClient$sendAck, ws, ostate1.snapshotId);
									return A2(
										elm$core$Debug$log,
										'TransitionToNextTrick',
										_Utils_Tuple2(
											_Utils_update(
												model,
												{state: state1}),
											cmd));
								} else {
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												message: _Utils_Tuple2(
													author$project$Common$Warning,
													'unexpected message (InRoom): ' + author$project$Main$showMessage(msg))
											}),
										elm$core$Platform$Cmd$none);
								}
							} else {
								break _n20$17;
							}
						case 'ReceiveNotification':
							if (_n20.b.a.$ === 'Err') {
								var err = _n20.b.a.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											message: _Utils_Tuple2(
												author$project$Common$Warning,
												'invalid notification: ' + elm$json$Json$Decode$errorToString(err))
										}),
									elm$core$Platform$Cmd$none);
							} else {
								switch (_n20.b.a.a.$) {
									case 'NotifyComment':
										var comment = _n20.b.a.a.a;
										var pstate1 = _Utils_update(
											pstate0,
											{
												logs: _Utils_ap(
													pstate0.logs,
													_List_fromArray(
														[
															author$project$Models$LogComment(comment)
														]))
											});
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													state: A5(author$project$Common$InRoom, ws, user, pstate1, indices0, chatTextInput0)
												}),
											elm$core$Platform$Cmd$none);
									case 'NotifyEntered':
										var userIdEntered = _n20.b.a.a.a;
										var pstate1 = _Utils_update(
											pstate0,
											{
												logs: _Utils_ap(
													pstate0.logs,
													_List_fromArray(
														[
															author$project$Models$LogEntered(userIdEntered)
														]))
											});
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													state: A5(author$project$Common$InRoom, ws, user, pstate1, indices0, chatTextInput0)
												}),
											elm$core$Platform$Cmd$none);
									case 'NotifyExited':
										var userIdExited = _n20.b.a.a.a;
										var pstate1 = _Utils_update(
											pstate0,
											{
												logs: _Utils_ap(
													pstate0.logs,
													_List_fromArray(
														[
															author$project$Models$LogExited(userIdExited)
														]))
											});
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{
													state: A5(author$project$Common$InRoom, ws, user, pstate1, indices0, chatTextInput0)
												}),
											elm$core$Platform$Cmd$none);
									case 'NotifyGameStart':
										var ostate0 = _n20.b.a.a.a;
										var cmd = A2(author$project$WebSocketClient$sendAck, ws, ostate0.snapshotId);
										var ostate1 = _Utils_update(
											ostate0,
											{synchronizing: true});
										var pstate1 = _Utils_update(
											pstate0,
											{
												game: author$project$Models$PlayingGame(ostate1),
												logs: _Utils_ap(
													pstate0.logs,
													_List_fromArray(
														[author$project$Models$LogGameStart]))
											});
										var state1 = A5(author$project$Common$InRoom, ws, user, pstate1, indices0, chatTextInput0);
										return A2(
											elm$core$Debug$log,
											'NotifyGameStart (+)',
											_Utils_Tuple2(
												_Utils_update(
													model,
													{state: state1}),
												cmd));
									case 'NotifySubmission':
										if (_n20.a.$ === 'PlayingGame') {
											var ostate0 = _n20.a.a;
											var submission = _n20.b.a.a.a;
											var newState = submission.newState;
											var maybeNext = function () {
												if (ostate0.synchronizing) {
													return elm$core$Maybe$Nothing;
												} else {
													var _n25 = _Utils_Tuple2(submission.trickLast, ostate0.observableInning);
													if (_n25.a.$ === 'Just') {
														if (_n25.b.$ === 'ObservableDuringInning') {
															var lastTable = _n25.a.a;
															var oinning0 = _n25.b.a;
															var ostate1 = _Utils_update(
																ostate0,
																{
																	observableInning: author$project$Models$ObservableDuringInning(
																		_Utils_update(
																			oinning0,
																			{table: lastTable})),
																	synchronizing: true
																});
															var state1 = A5(
																author$project$Common$InRoom,
																ws,
																user,
																_Utils_update(
																	pstate0,
																	{
																		game: author$project$Models$PlayingGame(ostate1)
																	}),
																indices0,
																chatTextInput0);
															var cmd = A2(
																author$project$Common$sendAfter,
																author$project$Constants$trickLastTimeMs,
																author$project$Common$TransitionToNextTrick(newState));
															return elm$core$Maybe$Just(
																_Utils_Tuple2(
																	_Utils_update(
																		model,
																		{state: state1}),
																	cmd));
														} else {
															return elm$core$Maybe$Nothing;
														}
													} else {
														var _n26 = _n25.a;
														var ostate1 = _Utils_update(
															newState,
															{synchronizing: true});
														var state1 = A5(
															author$project$Common$InRoom,
															ws,
															user,
															_Utils_update(
																pstate0,
																{
																	game: author$project$Models$PlayingGame(ostate1)
																}),
															indices0,
															chatTextInput0);
														var cmd = A2(author$project$WebSocketClient$sendAck, ws, ostate1.snapshotId);
														return elm$core$Maybe$Just(
															_Utils_Tuple2(
																_Utils_update(
																	model,
																	{state: state1}),
																cmd));
													}
												}
											}();
											if (maybeNext.$ === 'Just') {
												var next = maybeNext.a;
												return A2(elm$core$Debug$log, 'NotifySubmission (+)', next);
											} else {
												var message = _Utils_Tuple2(
													author$project$Common$Warning,
													'unexpected message (InRoom): ' + author$project$Main$showMessage(msg));
												return _Utils_Tuple2(
													_Utils_update(
														model,
														{message: message}),
													elm$core$Platform$Cmd$none);
											}
										} else {
											break _n20$17;
										}
									default:
										if (_n20.a.$ === 'PlayingGame') {
											var ostate0 = _n20.a.a;
											var _n31 = _n20.b.a.a;
											if (ostate0.synchronizing) {
												var ostate1 = _Utils_update(
													ostate0,
													{synchronizing: false});
												var state1 = A5(
													author$project$Common$InRoom,
													ws,
													user,
													_Utils_update(
														pstate0,
														{
															game: author$project$Models$PlayingGame(ostate1)
														}),
													indices0,
													chatTextInput0);
												return A2(
													elm$core$Debug$log,
													'NotifyNextStep (-)',
													_Utils_Tuple2(
														_Utils_update(
															model,
															{state: state1}),
														elm$core$Platform$Cmd$none));
											} else {
												return _Utils_Tuple2(
													_Utils_update(
														model,
														{
															message: _Utils_Tuple2(
																author$project$Common$Warning,
																'unexpected message (InRoom): ' + author$project$Main$showMessage(msg))
														}),
													elm$core$Platform$Cmd$none);
											}
										} else {
											break _n20$17;
										}
								}
							}
						case 'SendRequest':
							switch (_n20.b.a.$) {
								case 'SendChat':
									var _n23 = _n20.b.a;
									var cmd = A2(author$project$WebSocketClient$sendChat, ws, chatTextInput0);
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												state: A5(author$project$Common$InRoom, ws, user, pstate0, indices0, '')
											}),
										cmd);
								case 'SubmitCards':
									if (_n20.a.$ === 'PlayingGame') {
										var ostate0 = _n20.a.a;
										var _n32 = _n20.b.a;
										var _n33 = ostate0.observableInning;
										if (_n33.$ === 'ObservableDuringInning') {
											var inning = _n33.a;
											var ostate1 = _Utils_update(
												ostate0,
												{synchronizing: true});
											var indices1 = elm$core$Set$empty;
											var state1 = A5(
												author$project$Common$InRoom,
												ws,
												user,
												_Utils_update(
													pstate0,
													{
														game: author$project$Models$PlayingGame(ostate1)
													}),
												indices1,
												chatTextInput0);
											var cards = A2(author$project$Main$pickupSelectedCards, indices0, inning.yourHand);
											var cmd = A4(author$project$HttpClient$submitCards, model.origin, user.userId, pstate0.room.roomId, cards);
											return A2(
												elm$core$Debug$log,
												'SubmitCards (+)',
												_Utils_Tuple2(
													_Utils_update(
														model,
														{state: state1}),
													cmd));
										} else {
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{
														message: _Utils_Tuple2(author$project$Common$Warning, 'inning has already ended')
													}),
												elm$core$Platform$Cmd$none);
										}
									} else {
										break _n20$17;
									}
								case 'RequireNextInning':
									if (_n20.a.$ === 'PlayingGame') {
										var ostate0 = _n20.a.a;
										var _n34 = _n20.b.a;
										var cmd = A2(author$project$WebSocketClient$requireNextInning, ws, ostate0.snapshotId);
										var ostate1 = _Utils_update(
											ostate0,
											{synchronizing: true});
										var state1 = A5(
											author$project$Common$InRoom,
											ws,
											user,
											_Utils_update(
												pstate0,
												{
													game: author$project$Models$PlayingGame(ostate1)
												}),
											indices0,
											chatTextInput0);
										return A2(
											elm$core$Debug$log,
											'RequireNextInning (+)',
											_Utils_Tuple2(
												_Utils_update(
													model,
													{state: state1}),
												cmd));
									} else {
										break _n20$17;
									}
								default:
									break _n20$17;
							}
						case 'SelectCard':
							if (_n20.a.$ === 'PlayingGame') {
								var index = _n20.b.a;
								var indices1 = A2(elm$core$Set$insert, index, indices0);
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											state: A5(author$project$Common$InRoom, ws, user, pstate0, indices1, chatTextInput0)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								break _n20$17;
							}
						case 'UnselectCard':
							if (_n20.a.$ === 'PlayingGame') {
								var index = _n20.b.a;
								var indices1 = A2(elm$core$Set$remove, index, indices0);
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											state: A5(author$project$Common$InRoom, ws, user, pstate0, indices1, chatTextInput0)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								break _n20$17;
							}
						default:
							break _n20$17;
					}
				}
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							message: _Utils_Tuple2(
								author$project$Common$Warning,
								'unexpected message (InRoom): ' + author$project$Main$showMessage(msg))
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var author$project$Common$CreateUser = {$: 'CreateUser'};
var author$project$Common$SendRequest = function (a) {
	return {$: 'SendRequest', a: a};
};
var author$project$Common$UpdateInput = function (a) {
	return {$: 'UpdateInput', a: a};
};
var author$project$Common$UserNameInput = function (a) {
	return {$: 'UserNameInput', a: a};
};
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$View$viewEntrance = F2(
	function (_n0, userNameInput) {
		var level = _n0.a;
		var message = _n0.b;
		var sty = function () {
			if (level.$ === 'Information') {
				return A2(elm$html$Html$Attributes$style, 'color', 'gray');
			} else {
				return A2(elm$html$Html$Attributes$style, 'color', 'red');
			}
		}();
		return _List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[sty]),
						_List_fromArray(
							[
								elm$html$Html$text(message)
							]))
					])),
				A2(
				elm$html$Html$input,
				_List_fromArray(
					[
						elm$html$Html$Attributes$type_('text'),
						elm$html$Html$Attributes$placeholder('ユーザ名'),
						elm$html$Html$Attributes$value(userNameInput),
						elm$html$Html$Events$onInput(
						A2(elm$core$Basics$composeL, author$project$Common$UpdateInput, author$project$Common$UserNameInput))
					]),
				_List_Nil),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Common$SendRequest(author$project$Common$CreateUser))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('開始')
					]))
			]);
	});
var author$project$Common$CreateRoom = {$: 'CreateRoom'};
var author$project$Common$RoomNameInput = function (a) {
	return {$: 'RoomNameInput', a: a};
};
var author$project$Common$EnterRoom = function (a) {
	return {$: 'EnterRoom', a: a};
};
var elm$html$Html$li = _VirtualDom_node('li');
var elm$html$Html$ul = _VirtualDom_node('ul');
var author$project$View$viewRoomList = function (maybeRoomSummaries) {
	if (maybeRoomSummaries.$ === 'Nothing') {
		return A2(
			elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					elm$html$Html$text('(Rooms will be displayed here)')
				]));
	} else {
		var roomSummaries = maybeRoomSummaries.a;
		var elems = A2(
			elm$core$List$map,
			function (roomSummary) {
				var room = roomSummary.room;
				var members = A2(
					elm$core$String$join,
					', ',
					A2(
						elm$core$List$map,
						function (u) {
							return u.userName;
						},
						roomSummary.members));
				return A2(
					elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(room.roomName + (' (部屋ID: ' + (room.roomId + (', 参加者: ' + (members + ')'))))),
							A2(
							elm$html$Html$button,
							_List_fromArray(
								[
									elm$html$Html$Events$onClick(
									author$project$Common$SendRequest(
										author$project$Common$EnterRoom(room.roomId)))
								]),
							_List_fromArray(
								[
									elm$html$Html$text('参加')
								]))
						]));
			},
			roomSummaries);
		return A2(elm$html$Html$ul, _List_Nil, elems);
	}
};
var author$project$View$viewPlaza = F4(
	function (_n0, user, roomNameInput, maybeRoomSummaries) {
		var level = _n0.a;
		var message = _n0.b;
		var sty = function () {
			if (level.$ === 'Information') {
				return A2(elm$html$Html$Attributes$style, 'color', 'gray');
			} else {
				return A2(elm$html$Html$Attributes$style, 'color', 'red');
			}
		}();
		return _List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[sty]),
						_List_fromArray(
							[
								elm$html$Html$text(message)
							]))
					])),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text('ようこそ，' + (user.userName + (' さん (ユーザID: ' + (user.userId + ')'))))
					])),
				author$project$View$viewRoomList(maybeRoomSummaries),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('text'),
								elm$html$Html$Attributes$placeholder('部屋名'),
								elm$html$Html$Attributes$value(roomNameInput),
								elm$html$Html$Events$onInput(
								A2(elm$core$Basics$composeL, author$project$Common$UpdateInput, author$project$Common$RoomNameInput))
							]),
						_List_Nil),
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Events$onClick(
								author$project$Common$SendRequest(author$project$Common$CreateRoom))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('作成')
							]))
					]))
			]);
	});
var author$project$Common$ChatInput = function (a) {
	return {$: 'ChatInput', a: a};
};
var author$project$Common$SendChat = {$: 'SendChat'};
var author$project$PerSeat$find = F2(
	function (f, p) {
		return f(p.east) ? elm$core$Maybe$Just(0) : (f(p.south) ? elm$core$Maybe$Just(1) : (f(p.west) ? elm$core$Maybe$Just(2) : (f(p.north) ? elm$core$Maybe$Just(3) : elm$core$Maybe$Nothing)));
	});
var author$project$Game$getMySeat = F2(
	function (userId, ostate) {
		return A2(
			author$project$PerSeat$find,
			function (player) {
				return _Utils_eq(player.user.userId, userId);
			},
			ostate.meta.players);
	});
var author$project$Game$getExposedSize = function (exposed) {
	return 1 + elm$core$List$length(exposed.subsequent);
};
var author$project$Game$getTableSize = function (table) {
	switch (table.$) {
		case 'Starting':
			return 0;
		case 'TrickWuzun':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickWenzun':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickSingleWen':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickSingleWu':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickDoubleWen':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickDoubleWu':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickDoubleBoth':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickTripleWen':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		case 'TrickTripleWu':
			var e = table.a;
			return author$project$Game$getExposedSize(e);
		default:
			var e = table.a;
			return author$project$Game$getExposedSize(e);
	}
};
var elm$core$Basics$modBy = _Basics_modBy;
var author$project$PerSeat$advanceSeat = F2(
	function (seat, n) {
		return A2(elm$core$Basics$modBy, 4, seat + n);
	});
var author$project$Game$getNextSubmitterSeat = function (oinning) {
	var tableSize = author$project$Game$getTableSize(oinning.table);
	return (tableSize === 4) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(
		A2(author$project$PerSeat$advanceSeat, oinning.startsAt, tableSize));
};
var author$project$Game$isMyTurn = F2(
	function (userId, ostate) {
		var _n0 = A2(author$project$Game$getMySeat, userId, ostate);
		if (_n0.$ === 'Nothing') {
			return false;
		} else {
			var mySeat = _n0.a;
			var _n1 = ostate.observableInning;
			if (_n1.$ === 'ObservableInningEnd') {
				return false;
			} else {
				var oinning = _n1.a;
				var _n2 = author$project$Game$getNextSubmitterSeat(oinning);
				if (_n2.$ === 'Nothing') {
					return false;
				} else {
					var nextSubmitterSeat = _n2.a;
					return _Utils_eq(nextSubmitterSeat, mySeat);
				}
			}
		}
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var author$project$View$viewGridScheme = function (gridScheme) {
	return _List_fromArray(
		[
			A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('grid-container')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('grid-element-header')
						]),
					gridScheme.header),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('grid-element-left')
						]),
					gridScheme.left),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('grid-element-center')
						]),
					gridScheme.center),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('grid-element-right')
						]),
					gridScheme.right),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('grid-element-footer')
						]),
					gridScheme.footer)
				]))
		]);
};
var author$project$View$viewPlayer = F2(
	function (direction, player) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('player-frame')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('player-name')
						]),
					_List_fromArray(
						[
							elm$html$Html$text(direction + (' ' + player.user.userName))
						])),
					A2(
					elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(
							'得点： ' + elm$core$String$fromInt(player.score))
						]))
				]));
	});
var author$project$Common$RequireNextInning = {$: 'RequireNextInning'};
var author$project$Constants$svgButtonWidth = 70;
var author$project$Constants$goToNextButtonX = 350 - ((author$project$Constants$svgButtonWidth / 2) | 0);
var author$project$Constants$svgButtonHeight = 40;
var author$project$Constants$goToNextButtonY = 340 - ((author$project$Constants$svgButtonHeight / 2) | 0);
var author$project$Constants$svgHeight = 670;
var author$project$Constants$svgWidth = 700;
var author$project$Constants$svgButtonTextDepth = 25;
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$rect = elm$svg$Svg$trustedNode('rect');
var elm$svg$Svg$text = elm$virtual_dom$VirtualDom$text;
var elm$svg$Svg$text_ = elm$svg$Svg$trustedNode('text');
var elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var elm$svg$Svg$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$ViewTable$displayButton = F5(
	function (enabled, msg, text, x, y) {
		var stySize = _List_fromArray(
			[
				elm$svg$Svg$Attributes$x(
				elm$core$String$fromInt(x)),
				elm$svg$Svg$Attributes$y(
				elm$core$String$fromInt(y)),
				elm$svg$Svg$Attributes$width(
				elm$core$String$fromInt(author$project$Constants$svgButtonWidth)),
				elm$svg$Svg$Attributes$height(
				elm$core$String$fromInt(author$project$Constants$svgButtonHeight))
			]);
		return enabled ? A2(
			elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$rect,
					_Utils_ap(
						stySize,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$class('svg-button-enabled')
							])),
					_List_Nil),
					A2(
					elm$svg$Svg$text_,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$x(
							elm$core$String$fromInt(x + ((author$project$Constants$svgButtonWidth / 2) | 0))),
							elm$svg$Svg$Attributes$y(
							elm$core$String$fromInt(y + author$project$Constants$svgButtonTextDepth)),
							elm$svg$Svg$Attributes$textAnchor('middle'),
							elm$svg$Svg$Attributes$class('svg-button-text-enabled')
						]),
					_List_fromArray(
						[
							elm$svg$Svg$text(text)
						])),
					A2(
					elm$svg$Svg$rect,
					_Utils_ap(
						stySize,
						_List_fromArray(
							[
								elm$svg$Svg$Events$onClick(msg),
								elm$svg$Svg$Attributes$class('svg-button-front')
							])),
					_List_Nil)
				])) : A2(
			elm$svg$Svg$g,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$rect,
					_Utils_ap(
						stySize,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$class('svg-button-disabled')
							])),
					_List_Nil),
					A2(
					elm$svg$Svg$text_,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$x(
							elm$core$String$fromInt(x + ((author$project$Constants$svgButtonWidth / 2) | 0))),
							elm$svg$Svg$Attributes$y(
							elm$core$String$fromInt(y + author$project$Constants$svgButtonTextDepth)),
							elm$svg$Svg$Attributes$textAnchor('middle'),
							elm$svg$Svg$Attributes$class('svg-button-text-disabled')
						]),
					_List_fromArray(
						[
							elm$svg$Svg$text(text)
						]))
				]));
	});
var author$project$Constants$frontPileX = 90;
var author$project$Constants$frontPileY = 20;
var author$project$Constants$horizontalTileTopHeight = 34;
var author$project$Constants$leftPileX = 21;
var author$project$Constants$leftPileY = 570;
var author$project$Constants$rightPileX = 575;
var author$project$Constants$rightPileY = 100;
var author$project$Constants$selfPileX = 610;
var author$project$Constants$selfPileY = 495;
var author$project$Constants$verticalTileImageWidth = 40;
var author$project$Constants$tileThickness = 16;
var author$project$Constants$horizontalClosedCardPath = 'assets/closeh.png';
var elm$svg$Svg$image = elm$svg$Svg$trustedNode('image');
var elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var author$project$ViewTable$displayHorizontalClosedCard = F2(
	function (x, y) {
		return A2(
			elm$svg$Svg$image,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x(
					elm$core$String$fromInt(x)),
					elm$svg$Svg$Attributes$y(
					elm$core$String$fromInt(y)),
					elm$svg$Svg$Attributes$xlinkHref(author$project$Constants$horizontalClosedCardPath)
				]),
			_List_Nil);
	});
var author$project$Constants$stringifyWen = function (wen) {
	return 'wen' + elm$core$String$fromInt(wen);
};
var author$project$Constants$stringifyWu = function (wu) {
	return 'wu' + (elm$core$String$fromInt(wu.number) + (wu.design ? 't' : 'f'));
};
var author$project$Constants$stringifyCard = function (card) {
	if (card.$ === 'Wen') {
		var wen = card.a;
		return author$project$Constants$stringifyWen(wen);
	} else {
		var wu = card.a;
		return author$project$Constants$stringifyWu(wu);
	}
};
var author$project$Constants$horizontalOpenCardPath = function (card) {
	return 'assets/openh-' + (author$project$Constants$stringifyCard(card) + '.png');
};
var author$project$ViewTable$displayHorizontalOpenCard = F3(
	function (card, x, y) {
		return A2(
			elm$svg$Svg$image,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x(
					elm$core$String$fromInt(x)),
					elm$svg$Svg$Attributes$y(
					elm$core$String$fromInt(y)),
					elm$svg$Svg$Attributes$xlinkHref(
					author$project$Constants$horizontalOpenCardPath(card))
				]),
			_List_Nil);
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var author$project$ViewTable$displayHorizontalPile = F3(
	function (x0, y0, gains) {
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, card) {
						var y = y0 + (author$project$Constants$horizontalTileTopHeight * index);
						return _List_fromArray(
							[
								A2(author$project$ViewTable$displayHorizontalClosedCard, x0, y + (author$project$Constants$tileThickness * 3)),
								A2(author$project$ViewTable$displayHorizontalClosedCard, x0, y + (author$project$Constants$tileThickness * 2)),
								A2(author$project$ViewTable$displayHorizontalClosedCard, x0, y + author$project$Constants$tileThickness),
								A3(author$project$ViewTable$displayHorizontalOpenCard, card, x0, y)
							]);
					}),
				gains));
	});
var author$project$Constants$verticalClosedCardPath = 'assets/closev.png';
var author$project$ViewTable$displayVerticalClosedCard = F2(
	function (x, y) {
		return A2(
			elm$svg$Svg$image,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x(
					elm$core$String$fromInt(x)),
					elm$svg$Svg$Attributes$y(
					elm$core$String$fromInt(y)),
					elm$svg$Svg$Attributes$xlinkHref(author$project$Constants$verticalClosedCardPath)
				]),
			_List_Nil);
	});
var author$project$Constants$verticalOpenCardPath = function (card) {
	return 'assets/openv-' + (author$project$Constants$stringifyCard(card) + '.png');
};
var author$project$ViewTable$displayVerticalOpenCard = F3(
	function (card, x, y) {
		return A2(
			elm$svg$Svg$image,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x(
					elm$core$String$fromInt(x)),
					elm$svg$Svg$Attributes$y(
					elm$core$String$fromInt(y)),
					elm$svg$Svg$Attributes$xlinkHref(
					author$project$Constants$verticalOpenCardPath(card))
				]),
			_List_Nil);
	});
var author$project$ViewTable$displayVerticalPile = F3(
	function (x0, y0, gains) {
		return elm$core$List$concat(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, card) {
						var x = x0 + (author$project$Constants$verticalTileImageWidth * index);
						return _List_fromArray(
							[
								A2(author$project$ViewTable$displayVerticalClosedCard, x, y0 + (author$project$Constants$tileThickness * 3)),
								A2(author$project$ViewTable$displayVerticalClosedCard, x, y0 + (author$project$Constants$tileThickness * 2)),
								A2(author$project$ViewTable$displayVerticalClosedCard, x, y0 + author$project$Constants$tileThickness),
								A3(author$project$ViewTable$displayVerticalOpenCard, card, x, y0)
							]);
					}),
				gains));
	});
var author$project$ViewTable$displayGains = function (relQuad) {
	var selfGains = relQuad.self.gains;
	var selfX = author$project$Constants$selfPileX - (author$project$Constants$verticalTileImageWidth * elm$core$List$length(selfGains));
	var leftGains = relQuad.left.gains;
	var leftY = author$project$Constants$leftPileY - (author$project$Constants$horizontalTileTopHeight * elm$core$List$length(leftGains));
	return elm$core$List$concat(
		_List_fromArray(
			[
				A3(author$project$ViewTable$displayVerticalPile, selfX, author$project$Constants$selfPileY, selfGains),
				A3(author$project$ViewTable$displayHorizontalPile, author$project$Constants$rightPileX, author$project$Constants$rightPileY, relQuad.right.gains),
				A3(author$project$ViewTable$displayVerticalPile, author$project$Constants$frontPileX, author$project$Constants$frontPileY, relQuad.front.gains),
				A3(author$project$ViewTable$displayHorizontalPile, author$project$Constants$leftPileX, leftY, leftGains)
			]));
};
var author$project$Common$SubmitCards = {$: 'SubmitCards'};
var author$project$Common$getSelectedCards = F2(
	function (indices, cards) {
		return A2(
			elm$core$List$filterMap,
			function (_n0) {
				var index = _n0.a;
				var card = _n0.b;
				return A2(elm$core$Set$member, index, indices) ? elm$core$Maybe$Just(card) : elm$core$Maybe$Nothing;
			},
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, card) {
						return _Utils_Tuple2(index, card);
					}),
				cards));
	});
var author$project$Constants$decisionButtonGap = 15;
var author$project$Constants$decisionButtonY = 620 - author$project$Constants$svgButtonHeight;
var author$project$Constants$selectionShift = 10;
var author$project$Constants$selfHandX = 190;
var author$project$Constants$selfHandY = 515;
var author$project$Game$areTheSameBig = F2(
	function (wen, wu) {
		var _n0 = _Utils_Tuple2(wen, wu);
		_n0$4:
		while (true) {
			switch (_n0.a) {
				case 11:
					if (_n0.b === 9) {
						return true;
					} else {
						break _n0$4;
					}
				case 10:
					if (_n0.b === 8) {
						return true;
					} else {
						break _n0$4;
					}
				case 9:
					if (_n0.b === 7) {
						return true;
					} else {
						break _n0$4;
					}
				case 8:
					if (_n0.b === 5) {
						return true;
					} else {
						break _n0$4;
					}
				default:
					break _n0$4;
			}
		}
		return false;
	});
var elm$core$List$sortBy = _List_sortBy;
var author$project$Game$sortCards = elm$core$List$sortBy(
	function (card) {
		if (card.$ === 'Wen') {
			var wen = card.a;
			return wen;
		} else {
			var wu = card.a;
			return (100 + (wu.number * 2)) + (wu.design ? 1 : 0);
		}
	});
var author$project$Game$isStartable = function (cards) {
	var _n0 = author$project$Game$sortCards(cards);
	_n0$7:
	while (true) {
		if (_n0.b) {
			if (!_n0.b.b) {
				return true;
			} else {
				if (_n0.a.$ === 'Wu') {
					if ((_n0.b.a.$ === 'Wu') && (!_n0.b.b.b)) {
						var wu1 = _n0.a.a;
						var _n2 = _n0.b;
						var wu2 = _n2.a.a;
						return _Utils_eq(wu1.number, wu2.number) || ((wu1.number === 3) && (wu2.number === 6));
					} else {
						break _n0$7;
					}
				} else {
					if (_n0.b.a.$ === 'Wu') {
						if (!_n0.b.b.b) {
							var wen = _n0.a.a;
							var _n3 = _n0.b;
							var wu = _n3.a.a;
							return A2(author$project$Game$areTheSameBig, wen, wu.number);
						} else {
							if ((_n0.b.b.a.$ === 'Wu') && (!_n0.b.b.b.b)) {
								var wen = _n0.a.a;
								var _n6 = _n0.b;
								var wu1 = _n6.a.a;
								var _n7 = _n6.b;
								var wu2 = _n7.a.a;
								return _Utils_eq(wu1.number, wu2.number) && A2(author$project$Game$areTheSameBig, wen, wu1.number);
							} else {
								break _n0$7;
							}
						}
					} else {
						if (!_n0.b.b.b) {
							var wen1 = _n0.a.a;
							var _n1 = _n0.b;
							var wen2 = _n1.a.a;
							return _Utils_eq(wen1, wen2);
						} else {
							if (_n0.b.b.a.$ === 'Wu') {
								if (!_n0.b.b.b.b) {
									var wen1 = _n0.a.a;
									var _n4 = _n0.b;
									var wen2 = _n4.a.a;
									var _n5 = _n4.b;
									var wu = _n5.a.a;
									return _Utils_eq(wen1, wen2) && A2(author$project$Game$areTheSameBig, wen1, wu.number);
								} else {
									if ((_n0.b.b.b.a.$ === 'Wu') && (!_n0.b.b.b.b.b)) {
										var wen1 = _n0.a.a;
										var _n8 = _n0.b;
										var wen2 = _n8.a.a;
										var _n9 = _n8.b;
										var wu1 = _n9.a.a;
										var _n10 = _n9.b;
										var wu2 = _n10.a.a;
										return _Utils_eq(wen1, wen2) && (_Utils_eq(wu1.number, wu2.number) && A2(author$project$Game$areTheSameBig, wen1, wu1.number));
									} else {
										break _n0$7;
									}
								}
							} else {
								break _n0$7;
							}
						}
					}
				}
			}
		} else {
			break _n0$7;
		}
	}
	return false;
};
var author$project$Game$isSubmittable = F2(
	function (table, cards) {
		switch (table.$) {
			case 'Starting':
				return author$project$Game$isStartable(cards);
			case 'TrickWuzun':
				var e = table.a;
				return elm$core$List$length(cards) === 2;
			case 'TrickWenzun':
				var e = table.a;
				return elm$core$List$length(cards) === 2;
			case 'TrickSingleWen':
				var e = table.a;
				return elm$core$List$length(cards) === 1;
			case 'TrickSingleWu':
				var e = table.a;
				return elm$core$List$length(cards) === 1;
			case 'TrickDoubleWen':
				var e = table.a;
				return elm$core$List$length(cards) === 2;
			case 'TrickDoubleWu':
				var e = table.a;
				return elm$core$List$length(cards) === 2;
			case 'TrickDoubleBoth':
				var e = table.a;
				return elm$core$List$length(cards) === 2;
			case 'TrickTripleWen':
				var e = table.a;
				return elm$core$List$length(cards) === 3;
			case 'TrickTripleWu':
				var e = table.a;
				return elm$core$List$length(cards) === 3;
			default:
				var e = table.a;
				return elm$core$List$length(cards) === 4;
		}
	});
var author$project$ViewTable$Disabled = {$: 'Disabled'};
var author$project$ViewTable$NotSelected = {$: 'NotSelected'};
var author$project$ViewTable$Selected = {$: 'Selected'};
var author$project$Common$SelectCard = function (a) {
	return {$: 'SelectCard', a: a};
};
var author$project$Common$UnselectCard = function (a) {
	return {$: 'UnselectCard', a: a};
};
var author$project$Constants$standingCardPath = function (card) {
	return 'assets/stand-' + (author$project$Constants$stringifyCard(card) + '.png');
};
var elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var author$project$ViewTable$displayCardInHand = F5(
	function (index, cardState, card, x, y) {
		var sty = function () {
			switch (cardState.$) {
				case 'Disabled':
					return elm$svg$Svg$Attributes$opacity('0.8');
				case 'Selected':
					return elm$svg$Svg$Events$onClick(
						author$project$Common$UnselectCard(index));
				default:
					return elm$svg$Svg$Events$onClick(
						author$project$Common$SelectCard(index));
			}
		}();
		return A2(
			elm$svg$Svg$image,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$x(
					elm$core$String$fromInt(x)),
					elm$svg$Svg$Attributes$y(
					elm$core$String$fromInt(y)),
					sty,
					elm$svg$Svg$Attributes$xlinkHref(
					author$project$Constants$standingCardPath(card))
				]),
			_List_Nil);
	});
var author$project$ViewTable$displayHand = F2(
	function (handInfo, cards) {
		var svgsCard = function () {
			if (handInfo.synchronizing) {
				return A2(
					elm$core$List$indexedMap,
					F2(
						function (index, card) {
							return A5(author$project$ViewTable$displayCardInHand, index, author$project$ViewTable$Disabled, card, author$project$Constants$selfHandX + (author$project$Constants$verticalTileImageWidth * index), author$project$Constants$selfHandY);
						}),
					cards);
			} else {
				var _n2 = handInfo.maybeIndices;
				if (_n2.$ === 'Nothing') {
					return A2(
						elm$core$List$indexedMap,
						F2(
							function (index, card) {
								return A5(author$project$ViewTable$displayCardInHand, index, author$project$ViewTable$Disabled, card, author$project$Constants$selfHandX + (author$project$Constants$verticalTileImageWidth * index), author$project$Constants$selfHandY);
							}),
						cards);
				} else {
					var indices = _n2.a;
					return A2(
						elm$core$List$indexedMap,
						F2(
							function (index, card) {
								return A2(elm$core$Set$member, index, indices) ? A5(author$project$ViewTable$displayCardInHand, index, author$project$ViewTable$Selected, card, author$project$Constants$selfHandX + (author$project$Constants$verticalTileImageWidth * index), author$project$Constants$selfHandY - author$project$Constants$selectionShift) : A5(author$project$ViewTable$displayCardInHand, index, author$project$ViewTable$NotSelected, card, author$project$Constants$selfHandX + (author$project$Constants$verticalTileImageWidth * index), author$project$Constants$selfHandY);
							}),
						cards);
				}
			}
		}();
		var svgsButton = function () {
			var _n0 = handInfo.maybeIndices;
			if (_n0.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var indices = _n0.a;
				var selectedCards = A2(author$project$Common$getSelectedCards, indices, cards);
				var submittable = function () {
					var _n1 = handInfo.maybeTable;
					if (_n1.$ === 'Nothing') {
						return false;
					} else {
						var table = _n1.a;
						return A2(author$project$Game$isSubmittable, table, selectedCards);
					}
				}();
				var numberOfHandCards = elm$core$List$length(cards);
				return _List_fromArray(
					[
						A5(
						author$project$ViewTable$displayButton,
						submittable,
						author$project$Common$SendRequest(author$project$Common$SubmitCards),
						'決定',
						(author$project$Constants$selfHandX + (author$project$Constants$verticalTileImageWidth * numberOfHandCards)) + author$project$Constants$decisionButtonGap,
						author$project$Constants$decisionButtonY)
					]);
			}
		}();
		return _Utils_ap(svgsCard, svgsButton);
	});
var author$project$Constants$frontSubmittedX = 350;
var author$project$Constants$frontSubmittedY = 195;
var author$project$Constants$leftSubmittedX = 146;
var author$project$Constants$leftSubmittedY = 340;
var author$project$Constants$rightSubmittedX = 450;
var author$project$Constants$rightSubmittedY = 340;
var author$project$Constants$selfSubmittedX = 350;
var author$project$Constants$selfSubmittedY = 370;
var author$project$ViewTable$displayHorizontalSubmitted = F3(
	function (x, y, submitted) {
		var num = elm$core$List$length(submitted);
		var y0 = y - (((author$project$Constants$horizontalTileTopHeight * num) / 2) | 0);
		var _n0 = A3(
			elm$core$List$foldl,
			F2(
				function (cardOrClosed, _n1) {
					var index = _n1.a;
					var svgacc = _n1.b;
					var svg = function () {
						if (cardOrClosed.$ === 'Open') {
							var card = cardOrClosed.a;
							return A3(author$project$ViewTable$displayHorizontalOpenCard, card, x, y0 + (author$project$Constants$horizontalTileTopHeight * index));
						} else {
							return A2(author$project$ViewTable$displayHorizontalClosedCard, x, y0 + (author$project$Constants$horizontalTileTopHeight * index));
						}
					}();
					return _Utils_Tuple2(
						index + 1,
						A2(elm$core$List$cons, svg, svgacc));
				}),
			_Utils_Tuple2(0, _List_Nil),
			submitted);
		var svgacc0 = _n0.b;
		return elm$core$List$reverse(svgacc0);
	});
var author$project$ViewTable$displayVerticalSubmitted = F3(
	function (x, y, submitted) {
		var num = elm$core$List$length(submitted);
		var x0 = x - (((author$project$Constants$verticalTileImageWidth * num) / 2) | 0);
		var _n0 = A3(
			elm$core$List$foldl,
			F2(
				function (cardOrClosed, _n1) {
					var index = _n1.a;
					var svgacc = _n1.b;
					var svg = function () {
						if (cardOrClosed.$ === 'Open') {
							var card = cardOrClosed.a;
							return A3(author$project$ViewTable$displayVerticalOpenCard, card, x0 + (author$project$Constants$verticalTileImageWidth * index), y);
						} else {
							return A2(author$project$ViewTable$displayVerticalClosedCard, x0 + (author$project$Constants$verticalTileImageWidth * index), y);
						}
					}();
					return _Utils_Tuple2(
						index + 1,
						A2(elm$core$List$cons, svg, svgacc));
				}),
			_Utils_Tuple2(0, _List_Nil),
			submitted);
		var svgacc0 = _n0.b;
		return elm$core$List$reverse(svgacc0);
	});
var author$project$ViewTable$displayTable = function (relQuad) {
	return elm$core$List$concat(
		_List_fromArray(
			[
				A3(author$project$ViewTable$displayVerticalSubmitted, author$project$Constants$selfSubmittedX, author$project$Constants$selfSubmittedY, relQuad.self.submitted),
				A3(author$project$ViewTable$displayHorizontalSubmitted, author$project$Constants$rightSubmittedX, author$project$Constants$rightSubmittedY, relQuad.right.submitted),
				A3(author$project$ViewTable$displayVerticalSubmitted, author$project$Constants$frontSubmittedX, author$project$Constants$frontSubmittedY, relQuad.front.submitted),
				A3(author$project$ViewTable$displayHorizontalSubmitted, author$project$Constants$leftSubmittedX, author$project$Constants$leftSubmittedY, relQuad.left.submitted)
			]));
};
var author$project$ViewTable$makeRelativeQuad = F3(
	function (selfSeat, gainsQuad, submittedQuad) {
		var elem0 = {gains: gainsQuad.east, submitted: submittedQuad.east};
		var elem1 = {gains: gainsQuad.south, submitted: submittedQuad.south};
		var elem2 = {gains: gainsQuad.west, submitted: submittedQuad.west};
		var elem3 = {gains: gainsQuad.north, submitted: submittedQuad.north};
		var elemX = {gains: _List_Nil, submitted: _List_Nil};
		switch (selfSeat) {
			case 0:
				return {front: elem2, left: elem3, right: elem1, self: elem0};
			case 1:
				return {front: elem3, left: elem0, right: elem2, self: elem1};
			case 2:
				return {front: elem0, left: elem1, right: elem3, self: elem2};
			case 3:
				return {front: elem1, left: elem2, right: elem0, self: elem3};
			default:
				return {front: elemX, left: elemX, right: elemX, self: elemX};
		}
	});
var author$project$Game$bigToWenAndWu = function (big) {
	switch (big) {
		case 1:
			return _Utils_Tuple2(8, 5);
		case 2:
			return _Utils_Tuple2(9, 7);
		case 3:
			return _Utils_Tuple2(10, 8);
		case 4:
			return _Utils_Tuple2(11, 9);
		default:
			return _Utils_Tuple2(0, 0);
	}
};
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var author$project$Game$exposedToList = F3(
	function (n, f, e) {
		return A2(
			elm$core$List$map,
			function (xOrClosed) {
				if (xOrClosed.$ === 'Open') {
					var x = xOrClosed.a;
					return A2(
						elm$core$List$map,
						function (y) {
							return author$project$Models$Open(y);
						},
						f(x));
				} else {
					return A2(elm$core$List$repeat, n, author$project$Models$Closed);
				}
			},
			A2(
				elm$core$List$cons,
				author$project$Models$Open(e.first),
				e.subsequent));
	});
var author$project$Game$tableToCards = function (table) {
	switch (table.$) {
		case 'Starting':
			return _List_Nil;
		case 'TrickWuzun':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				2,
				function (u) {
					var _n1 = u;
					return _List_fromArray(
						[
							author$project$Models$Wu(
							{design: true, number: 3}),
							author$project$Models$Wu(
							{design: true, number: 6})
						]);
				},
				e);
		case 'TrickWenzun':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				2,
				function (b) {
					if (!b) {
						return _List_fromArray(
							[
								author$project$Models$Wen(1),
								author$project$Models$Wen(1)
							]);
					} else {
						return _List_fromArray(
							[
								author$project$Models$Wen(2),
								author$project$Models$Wen(2)
							]);
					}
				},
				e);
		case 'TrickSingleWen':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				1,
				function (wen) {
					return _List_fromArray(
						[
							author$project$Models$Wen(wen)
						]);
				},
				e);
		case 'TrickSingleWu':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				1,
				function (wu) {
					return _List_fromArray(
						[
							author$project$Models$Wu(wu)
						]);
				},
				e);
		case 'TrickDoubleWen':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				2,
				function (wen) {
					return _List_fromArray(
						[
							author$project$Models$Wen(wen),
							author$project$Models$Wen(wen)
						]);
				},
				e);
		case 'TrickDoubleWu':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				2,
				function (wunum) {
					return _List_fromArray(
						[
							author$project$Models$Wu(
							{design: true, number: wunum}),
							author$project$Models$Wu(
							{design: false, number: wunum})
						]);
				},
				e);
		case 'TrickDoubleBoth':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				2,
				function (bigd) {
					var _n3 = author$project$Game$bigToWenAndWu(bigd.main);
					var wen = _n3.a;
					var wunum = _n3.b;
					return _List_fromArray(
						[
							author$project$Models$Wen(wen),
							author$project$Models$Wu(
							{design: bigd.design, number: wunum})
						]);
				},
				e);
		case 'TrickTripleWen':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				3,
				function (bigd) {
					var _n4 = author$project$Game$bigToWenAndWu(bigd.main);
					var wen = _n4.a;
					var wunum = _n4.b;
					return _List_fromArray(
						[
							author$project$Models$Wen(wen),
							author$project$Models$Wen(wen),
							author$project$Models$Wu(
							{design: bigd.design, number: wunum})
						]);
				},
				e);
		case 'TrickTripleWu':
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				3,
				function (big) {
					var _n5 = author$project$Game$bigToWenAndWu(big);
					var wen = _n5.a;
					var wunum = _n5.b;
					return _List_fromArray(
						[
							author$project$Models$Wen(wen),
							author$project$Models$Wu(
							{design: true, number: wunum}),
							author$project$Models$Wu(
							{design: false, number: wunum})
						]);
				},
				e);
		default:
			var e = table.a;
			return A3(
				author$project$Game$exposedToList,
				4,
				function (big) {
					var _n6 = author$project$Game$bigToWenAndWu(big);
					var wen = _n6.a;
					var wunum = _n6.b;
					return _List_fromArray(
						[
							author$project$Models$Wen(wen),
							author$project$Models$Wen(wen),
							author$project$Models$Wu(
							{design: true, number: wunum}),
							author$project$Models$Wu(
							{design: false, number: wunum})
						]);
				},
				e);
	}
};
var author$project$ViewTable$makeSubmittedQuad = F2(
	function (startSeat, table) {
		var list0 = author$project$Game$tableToCards(table);
		var t = function () {
			if (!list0.b) {
				return {x0: _List_Nil, x1: _List_Nil, x2: _List_Nil, x3: _List_Nil};
			} else {
				if (!list0.b.b) {
					var x0 = list0.a;
					return {x0: x0, x1: _List_Nil, x2: _List_Nil, x3: _List_Nil};
				} else {
					if (!list0.b.b.b) {
						var x0 = list0.a;
						var _n2 = list0.b;
						var x1 = _n2.a;
						return {x0: x0, x1: x1, x2: _List_Nil, x3: _List_Nil};
					} else {
						if (!list0.b.b.b.b) {
							var x0 = list0.a;
							var _n3 = list0.b;
							var x1 = _n3.a;
							var _n4 = _n3.b;
							var x2 = _n4.a;
							return {x0: x0, x1: x1, x2: x2, x3: _List_Nil};
						} else {
							var x0 = list0.a;
							var _n5 = list0.b;
							var x1 = _n5.a;
							var _n6 = _n5.b;
							var x2 = _n6.a;
							var _n7 = _n6.b;
							var x3 = _n7.a;
							return {x0: x0, x1: x1, x2: x2, x3: x3};
						}
					}
				}
			}
		}();
		switch (startSeat) {
			case 0:
				return {east: t.x0, north: t.x3, south: t.x1, west: t.x2};
			case 1:
				return {east: t.x3, north: t.x2, south: t.x0, west: t.x1};
			case 2:
				return {east: t.x2, north: t.x1, south: t.x3, west: t.x0};
			case 3:
				return {east: t.x1, north: t.x0, south: t.x2, west: t.x3};
			default:
				return {east: _List_Nil, north: _List_Nil, south: _List_Nil, west: _List_Nil};
		}
	});
var elm$core$Basics$not = _Basics_not;
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var author$project$ViewTable$view = F4(
	function (userId, selfSeat, handInfo, observableInning) {
		if (observableInning.$ === 'ObservableDuringInning') {
			var oinning = observableInning.a;
			var yourHand = oinning.yourHand;
			var submittedQuad = A2(author$project$ViewTable$makeSubmittedQuad, oinning.startsAt, oinning.table);
			var relQuad = A3(author$project$ViewTable$makeRelativeQuad, selfSeat, oinning.gains, submittedQuad);
			return A2(
				elm$svg$Svg$svg,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$width(
						elm$core$String$fromInt(author$project$Constants$svgWidth)),
						elm$svg$Svg$Attributes$height(
						elm$core$String$fromInt(author$project$Constants$svgHeight)),
						elm$svg$Svg$Attributes$viewBox(
						'0 0 ' + (elm$core$String$fromInt(author$project$Constants$svgWidth) + (' ' + elm$core$String$fromInt(author$project$Constants$svgHeight))))
					]),
				elm$core$List$concat(
					_List_fromArray(
						[
							author$project$ViewTable$displayGains(relQuad),
							author$project$ViewTable$displayTable(relQuad),
							A2(author$project$ViewTable$displayHand, handInfo, yourHand)
						])));
		} else {
			var gainsQuad = observableInning.a;
			var submittedQuad = {east: _List_Nil, north: _List_Nil, south: _List_Nil, west: _List_Nil};
			var relQuad = A3(author$project$ViewTable$makeRelativeQuad, selfSeat, gainsQuad, submittedQuad);
			return A2(
				elm$svg$Svg$svg,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$width(
						elm$core$String$fromInt(author$project$Constants$svgWidth)),
						elm$svg$Svg$Attributes$height(
						elm$core$String$fromInt(author$project$Constants$svgHeight)),
						elm$svg$Svg$Attributes$viewBox(
						'0 0 ' + (elm$core$String$fromInt(author$project$Constants$svgWidth) + (' ' + elm$core$String$fromInt(author$project$Constants$svgHeight))))
					]),
				_Utils_ap(
					author$project$ViewTable$displayGains(relQuad),
					_List_fromArray(
						[
							A5(
							author$project$ViewTable$displayButton,
							!handInfo.synchronizing,
							author$project$Common$SendRequest(author$project$Common$RequireNextInning),
							'次へ',
							author$project$Constants$goToNextButtonX,
							author$project$Constants$goToNextButtonY)
						])));
		}
	});
var elm$html$Html$b = _VirtualDom_node('b');
var author$project$View$viewRoom = F5(
	function (_n0, user, pstate, indices, chatTextInput) {
		var level = _n0.a;
		var message = _n0.b;
		var room = pstate.room;
		var elemsChat = _List_fromArray(
			[
				A2(
				elm$html$Html$ul,
				_List_Nil,
				A2(
					elm$core$List$map,
					function (log) {
						switch (log.$) {
							case 'LogComment':
								var comment = log.a;
								return A2(
									elm$html$Html$li,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$b,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(comment.from.userName)
												])),
											elm$html$Html$text(': ' + comment.text)
										]));
							case 'LogEntered':
								var u = log.a;
								return A2(
									elm$html$Html$li,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$b,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(u.userName)
												])),
											elm$html$Html$text(' さんが参加しました')
										]));
							case 'LogExited':
								var u = log.a;
								return A2(
									elm$html$Html$li,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$b,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text(u.userName)
												])),
											elm$html$Html$text(' さんが退出しました')
										]));
							default:
								return A2(
									elm$html$Html$li,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$b,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('対局開始！')
												]))
										]));
						}
					},
					pstate.logs)),
				A2(
				elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$input,
								_List_fromArray(
									[
										elm$html$Html$Attributes$type_('text'),
										elm$html$Html$Attributes$placeholder('コメント'),
										elm$html$Html$Attributes$value(chatTextInput),
										elm$html$Html$Events$onInput(
										A2(elm$core$Basics$composeL, author$project$Common$UpdateInput, author$project$Common$ChatInput))
									]),
								_List_Nil),
								A2(
								elm$html$Html$button,
								_List_fromArray(
									[
										elm$html$Html$Events$onClick(
										author$project$Common$SendRequest(author$project$Common$SendChat))
									]),
								_List_fromArray(
									[
										elm$html$Html$text('送信')
									]))
							]))
					]))
			]);
		var stys = function () {
			if (level.$ === 'Information') {
				return _List_fromArray(
					[
						elm$html$Html$Attributes$class('grid-element-footer'),
						elm$html$Html$Attributes$class('footer-style-normal')
					]);
			} else {
				return _List_fromArray(
					[
						elm$html$Html$Attributes$class('grid-element-footer'),
						elm$html$Html$Attributes$class('footer-style-warning')
					]);
			}
		}();
		var _n1 = pstate.game;
		if (_n1.$ === 'WaitingStart') {
			var users = _n1.a;
			var members = A2(
				elm$core$String$join,
				', ',
				A2(
					elm$core$List$map,
					function (u) {
						return u.userName;
					},
					users));
			var elemsDebug = _List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							elm$html$Html$text(room.roomName + (' (部屋ID: ' + (room.roomId + (', 参加者: ' + (members + ')')))))
						]))
				]);
			return author$project$View$viewGridScheme(
				{
					center: _List_Nil,
					footer: _List_fromArray(
						[
							A2(
							elm$html$Html$div,
							stys,
							_List_fromArray(
								[
									elm$html$Html$text(message)
								]))
						]),
					header: _List_fromArray(
						[
							elm$html$Html$text('header')
						]),
					left: elemsDebug,
					right: elemsChat
				});
		} else {
			var ostate = _n1.a;
			var gameMeta = ostate.meta;
			var players = gameMeta.players;
			var synchronizing = ostate.synchronizing;
			var turn = A2(author$project$Game$isMyTurn, user.userId, ostate);
			var userId = user.userId;
			var _n2 = A2(
				author$project$PerSeat$find,
				function (p) {
					return _Utils_eq(p.user.userId, userId);
				},
				players);
			if (_n2.$ === 'Nothing') {
				return _List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('broken')
							]))
					]);
			} else {
				var seat = _n2.a;
				var maybeTable = function () {
					var _n3 = ostate.observableInning;
					if (_n3.$ === 'ObservableDuringInning') {
						var oinning = _n3.a;
						return elm$core$Maybe$Just(oinning.table);
					} else {
						return elm$core$Maybe$Nothing;
					}
				}();
				var handInfo = {
					maybeIndices: turn ? elm$core$Maybe$Just(indices) : elm$core$Maybe$Nothing,
					maybeTable: maybeTable,
					synchronizing: synchronizing
				};
				var elemsDebug = _List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('room ID: ' + room.roomId)
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('snapshot ID: ' + ostate.snapshotId)
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								'synchronizing: ' + (synchronizing ? 'Y' : 'N'))
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								'your turn: ' + (turn ? 'Y' : 'N'))
							]))
					]);
				var elemsLeft = _List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('room-name')
							]),
						_List_fromArray(
							[
								elm$html$Html$text(room.roomName)
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								elm$core$String$fromInt(gameMeta.inningIndex + 1) + ('局目，' + (elm$core$String$fromInt(gameMeta.numConsecutives - 1) + '本場')))
							])),
						A2(author$project$View$viewPlayer, '東', players.east),
						A2(author$project$View$viewPlayer, '南', players.south),
						A2(author$project$View$viewPlayer, '西', players.west),
						A2(author$project$View$viewPlayer, '北', players.north),
						A2(elm$html$Html$div, _List_Nil, elemsDebug)
					]);
				return author$project$View$viewGridScheme(
					{
						center: _List_fromArray(
							[
								A4(author$project$ViewTable$view, userId, seat, handInfo, ostate.observableInning)
							]),
						footer: _List_fromArray(
							[
								A2(
								elm$html$Html$div,
								stys,
								_List_fromArray(
									[
										elm$html$Html$text(message)
									]))
							]),
						header: _List_fromArray(
							[
								elm$html$Html$text('header')
							]),
						left: elemsLeft,
						right: elemsChat
					});
			}
		}
	});
var author$project$View$viewBody = function (model) {
	var message = model.message;
	var _n0 = model.state;
	switch (_n0.$) {
		case 'AtEntrance':
			var userNameInput = _n0.a;
			return A2(author$project$View$viewEntrance, message, userNameInput);
		case 'AtPlaza':
			var user = _n0.b;
			var roomNameInput = _n0.c;
			var maybeRoomSummaries = _n0.d;
			return A4(author$project$View$viewPlaza, message, user, roomNameInput, maybeRoomSummaries);
		default:
			var user = _n0.b;
			var personalState = _n0.c;
			var indices = _n0.d;
			var chatTextInput = _n0.e;
			return A5(author$project$View$viewRoom, message, user, personalState, indices, chatTextInput);
	}
};
var author$project$Main$view = function (model) {
	return {
		body: author$project$View$viewBody(model),
		title: 'tianjiupai'
	};
};
var elm$browser$Browser$document = _Browser_document;
var author$project$Main$main = elm$browser$Browser$document(
	{init: author$project$Main$init, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	A2(
		elm$json$Json$Decode$andThen,
		function (windowWidth) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (windowHeight) {
					return A2(
						elm$json$Json$Decode$andThen,
						function (user) {
							return A2(
								elm$json$Json$Decode$andThen,
								function (httpOrigin) {
									return elm$json$Json$Decode$succeed(
										{httpOrigin: httpOrigin, user: user, windowHeight: windowHeight, windowWidth: windowWidth});
								},
								A2(elm$json$Json$Decode$field, 'httpOrigin', elm$json$Json$Decode$string));
						},
						A2(elm$json$Json$Decode$field, 'user', elm$json$Json$Decode$string));
				},
				A2(elm$json$Json$Decode$field, 'windowHeight', elm$json$Json$Decode$int));
		},
		A2(elm$json$Json$Decode$field, 'windowWidth', elm$json$Json$Decode$int)))(0)}});}(this));