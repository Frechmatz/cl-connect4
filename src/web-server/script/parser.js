
/*
  Simple command line argument parser
*/
var Parser = function(paramStr) {
    var paramStr = paramStr ? paramStr.trim() : '';
    var params = paramStr.length ? paramStr.split(/\s/) : [];

    this.getNth = function(index) {
	return index < params.length ? params[index] : null;
    };

    /*
      --size 1 => '1'  
     */
    this.get = function(paramName) {
	var index = params.indexOf(paramName);
	return index != -1 ? this.getNth(index + 1) : null;
    };

    this.asInteger = function(param) {
	var i = Number.parseInt(param);
	return isNaN(i) ? null : i;
    };
};
