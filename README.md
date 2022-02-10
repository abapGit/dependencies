# DEPRECATED

use https://github.com/abaplint/abaplint-sci-client instead

# dependencies
~~Serialize dependencies of a package structure to git.~~

~~**Warning:** Do not specify the git repository of the package as the target!~~

~~For use in connection with static code analysis, method implementations of dependencies are excluded in the files.~~

### Example
* ~~ABAP development is done in package $PROJECT, and stored in repository https://github.com/user/project/~~
* ~~Create a new repository project_deps to store the dependencies~~
* ~~Run report with input $PROJECT and repository https://github.com/user/project_deps/, note that files in project_deps will be overwritten without any warnings~~
