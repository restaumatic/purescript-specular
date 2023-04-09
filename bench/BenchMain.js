export function exposeRunBenchmark(tests) {
  return function () {
    global.runBenchmark = (filters) => {
      console.log("filters:" + JSON.stringify(filters));
      tests
        .filter(
          (t) =>
            filters.length === 0 ||
            filters.some((f) => t.name.indexOf(f) !== -1)
        )
        .forEach(function (t) {
          for (var k = 0; k < 1; k++) {
            const allTimes = [];
            var M = 0;
            // Try a good M
            {
              const start = performance.now();
              while (performance.now() - start < 100) {
                t.fn();
                M++;
              }
            }
            var allStart = performance.now();
            while (performance.now() - allStart < 2000) {
              var start = performance.now();
              for (var j = 0; j < M; j++) {
                t.fn();
              }
              var elapsed = performance.now() - start;
              allTimes.push(elapsed);
            }
            allTimes.sort(); // for median
            const min = allTimes.reduce((a, b) => Math.min(a, b), Infinity);
            const max = allTimes.reduce((a, b) => Math.max(a, b), -Infinity);
            const mean = sum(allTimes) / allTimes.length;
            const stddev = Math.sqrt(
              sum(allTimes.map((x) => (x - mean) * (x - mean)))
            );
            const md = median(allTimes);
            const fmt = (t) => Math.round((1000000 * t) / M) / 1000;
            console.log(
              `${t.name}: min=${fmt(min)} mean=${fmt(mean)} median=${fmt(
                md
              )} max=${fmt(max)} stddev=${fmt(stddev)} (us/iter; ${
                allTimes.length
              } runs, ${M} iters/run)`
            );
          }
        });
    };
  };
}

const sum = (xs) => xs.reduce((a, b) => a + b, 0);

function median(xs) {
  if (xs.length % 2 == 0) {
    return (xs[Math.floor(xs.length / 2)] + xs[Math.ceil(xs.length / 2)]) / 2;
  } else {
    return xs[Math.floor(xs.length / 2)];
  }
}
