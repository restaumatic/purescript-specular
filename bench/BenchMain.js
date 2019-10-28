exports.runBenchmark = function(tests) {
  return function() {
    tests.forEach(function(t) {
      for(var k = 0; k < 1; k++) {
        var minTime = 99999;
        var M = 1000;
        var allStart = performance.now();
        var n = 0;
        while(performance.now() - allStart < 2000) {
          var start = performance.now();
          for(var j = 0; j < M; j++) {
            t.fn();
          }
          var elapsed = performance.now() - start;
          if(elapsed < minTime) {
            minTime = elapsed;
          }
          n += M;
        }
        console.log(t.name + ': ' + (Math.round(1000000 * minTime / M) / 1000) + ' us (' + n + ' runs)');
      }
    });
  };
};
