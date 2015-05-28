function visit(mapFun, resultFun, array) {
	if (_.isArray(array))
    return resultFun(_.map(array, mapFun));
  else
    return resultFun(array);
}

function partial1(fun, arg1) {
	return function(arg2) {
    return fun.call(null, arg1, arg2);
  };
}

function postDepth(fun, ary) {
  debugger;
	return visit(partial1(postDepth, fun), fun, ary);
}
