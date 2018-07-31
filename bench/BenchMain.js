global.Benchmark = require('benchmark');

exports.runBenchmark = function(tests) {
  return function() {
    var suite = new Benchmark.Suite();
    tests.forEach(function(test) {
      suite.add(test.name, test.fn);
    });
    suite.on('cycle', function(event) {
      console.log(String(event.target));
    })
    suite.run();
  };
};
