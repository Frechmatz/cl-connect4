




var Parser = function(paramStr) {
    var params = paramStr ? paramsStr.split(/\s/) : [];

    this.getNth = function(index) {
	return index + 1 < params.length ? params[index] : null;
    };

    this.get = function(paramName) {
	var index = params.indexOf(params);
	return index != -1 ? this.getNth(index + 1) : null;
    };

    this.asInteger = function(param) {
	var i = Number.parseInt(param);
	return isNaN(i) ? null : i;
    };
};
