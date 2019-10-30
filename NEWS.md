# Maeswrap 1.8.0

* Added a `NEWS.md` file to track changes to the package;  
* Much faster I/O functions using `data.table::fread()` and `data.table::fwrite()` (~ -300% execution time);  
* Documentation is now written using Markdown using `markdownify::roxygen2md()`;  
* Add `readtestflx()` to read the `testflx.dat` file (if any);  
* Add `readwatbalday()` to read the `watbalday.dat` file (if any);  
* Add a website using `pkgdown::build_site()`.
