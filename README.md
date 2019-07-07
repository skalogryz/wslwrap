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

    wslwrap.exe ls -l /mnt/d/wslwrap/trunk
    
The utility cannot and doesn't recognize relative paths (and would pass them as is, not trying to replace windows to unix slashes).
