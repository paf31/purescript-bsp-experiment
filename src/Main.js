
exports.atFps = function (v) {
    return function (f) {
	return function() {
	    window.setInterval(function() {
		f(new Date().getTime() / 1000.0)(); }, 1000.0/v);
	};
    };
}
