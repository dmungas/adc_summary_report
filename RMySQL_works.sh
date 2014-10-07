#	Taken from: http://andorian.blogspot.com/2013/10/rmysql-on-osx.html


# Find your R home
 
$ echo "R.home()" | Rscript /dev/stdin
Loading required package: stats
Loading required package: methods
[1] "/usr/local/Cellar/r/3.0.1/R.framework/Resources"

/Library/Frameworks/R.framework/Resources
 
 
# Make sure that you have the MySQL home var in your Renviron
 
$ fgrep MYSQL_HOME /Library/Frameworks/R.framework/Resources/etc/Renviron

MYSQL_HOME="/usr/local/mysql-5.6.21-osx10.8-x86_64/"

#	Add "MYSQL_HOME="/usr/local/mysql-5.6.21-osx10.8-x86_64/"" to Renviron is not present


 
#### This step trips up everyone ####
# Make sure that the libmysqlclient dynamic lib is in your R lib path by
 
# 1. Get the dll path
$ echo "Sys.getenv('DYLD_FALLBACK_LIBRARY_PATH')" | Rscript /dev/stdin
Loading required package: stats
Loading required package: methods
[1] "/usr/local/Cellar/r/3.0.1/R.framework/Resources/lib"

/Library/Frameworks/R.framework/Resources/lib

 
# 2. Symlink your libmysqlclient
$ ln -s /usr/local/mysql-5.6.21-osx10.8-x86_64/lib/libmysqlclient.18.dylib /Library/Frameworks/R.framework/Resources/lib
 
# 3. Install the package
$ export PKG_LIBS="-L/usr/local/mysql-5.6.21-osx10.8-x86_64/lib/ -lmysqlclient"
$ export PKG_CPPFLAGS="-I/usr/local/mysql-5.6.21-osx10.8-x86_64/include/"

# In R

Sys.setenv(PKG_CPPFLAGS = "-I/usr/local/mysql/include")
Sys.setenv(PKG_LIBS = "-L/usr/local/mysql/lib -lmysqlclient")

install.packages("~/Desktop/RMySQL_0.9-3.tar.gz", repos = NULL, type = "source")
...
...
** installing vignettes
** testing if installed package can be loaded
* DONE (RMySQL)
