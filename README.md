# wslwrap

It is a small utility that wraps and hides a call wsl.

There are two purposse of the utility are:
- to demonstrate an ability to execute a WSL (linux) application by Win32 Application
- wrap around (linux) FPC to let Win32 Lazarus compile Linux targets using WSL.

## Prerequisites
You need to have WSL installed and configured with a default distro.

WSL1 or WSL2 should not matter, but the utility was tested with WSL2 only.

## How to compile
Two ways, either compile using FPC - wslwrap.lpr 
or using lazarus IDE, by openning wslwrap.lpi

## How to use
The utility passes the all command-line parameters to the WSL.exe utility (with -e switch enabled. bypassing bash)
For example
     
     wslwrap.exe ls -l 
     
can produce and output: 
     
     D:\wslwrap\trunk>wslwrap.exe ls -l
     total 504
     -rwxrwxrwx 1 dmitry dmitry    276 Jul  7 09:38 README.md
     -rwxrwxrwx 1 dmitry dmitry 506030 Jul  7 09:28 wslwrap.exe
     -rwxrwxrwx 1 dmitry dmitry   1650 Jul  7 09:38 wslwrap.lpi
     -rwxrwxrwx 1 dmitry dmitry   2763 Jul  7 09:38 wslwrap.lpr

It is possible to set a specific execution target to be executed.
Change the name of the executable by adding underscore character to it. The name of the desired executable is defined by all characters following it.


For example. Rename "wslwrap.exe" to "wslwrap_ls.exe".  The new name indicates that "ls" executable must be used. It's no longer needed to specify it as one of the parameters.
Calling

    wslwrap_ls.exe -l

is exactly the same as

    wslwrap.exe ls -l

### Absolute path replacement     

If any of the parameter contains a sequence of %CHAR%:\ (i.e. C:\, D:\, etc...), such parameter would be treated as an absolute path and would be replaced accordingly.

The replacement is based on WSL logic, where all Windows paths are mounted to /mnt/%char% directory.

Thus a call

     wslwrap.exe ls -l D:\wslwrap\trunk

would be translated as

    ls -l /mnt/d/wslwrap/trunk
    
The utility cannot and doesn't recognize relative paths (and would pass them as is, not trying to replace windows to unix slashes).

### use of .wrp file

The file is used to specify the desired executable name (instead of using file renaming name), as well as being able to override link.res file paths (used as a hack for cross-compile binaries)
The .wrp file name must match the executable name, except for extention. (and executable name can be anything desired).

For example, if the executable name is x86_64-linux-ld.exe, then the .wrp file should be x86_64-linux-ld.wrp. The file must be located at the same directory as the executable.

The file format is a simple key=value files. Example:

    exe=ld
    linkres=1

where exe= specifies the executable to be called in WSL.
linkres= (if specified) would update the existing link.res file (by working directory) replacing all windows paths with WSL unix paths.
    
Creating your own ls.exe. Compile wslwrap.exe, rename it to ls.exe. Create ls.wrp file and add one line to it

   exe=ls
   
That's it. Now you can run your ls.exe (with any additional parameters desired)   
