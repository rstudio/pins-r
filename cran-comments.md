Currently, there is a NOTE for pins on Windows that looks like this:

```
		#STDOFF	2:05:08.9
		#STDOFF	8:05:43.2
		#STDOFF	7:36:41.7
		#STDOFF	-0:25:21.1
		#STDOFF	1:39:49.2
		#STDOFF	-0:36:44.68
		#STDOFF	-4:56:01.6
		#STDOFF	-3:58:29.2
		#STDOFF	-4:19:18.3
		#STDOFF	-5:36:13.3
		#STDOFF	-4:56:16.4
		#STDOFF	2:05:08.9
		#STDOFF	8:05:43.2
		#STDOFF	7:36:41.7
		#STDOFF	-0:25:21.1
		#STDOFF	1:39:49.2
		#STDOFF	-0:36:44.68
		#STDOFF	-4:56:01.6
		#STDOFF	-3:58:29.2
		#STDOFF	-4:19:18.3
		#STDOFF	-5:36:13.3
		#STDOFF	-4:56:16.4
```

This is not due to pins but rather to the [arrow](https://cran.r-project.org/package=arrow) package and a problem with [how it includes a time zone database](https://github.com/apache/arrow/issues/35594).

## revdepcheck results

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
