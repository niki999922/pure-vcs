# Use
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/fp-ctd-itmo/hw2-niki999922/blob/master/LICENSE)

This program allow you work with your file system "pure" without fear of lost state system, because of i't work like offline and in the end write result.

Program providing offline work with the file system with support for local version control. To do this, transfer the start directory from which the local file system will be initialized

## Run program: 
```shell script
stack exec hw2 -- <absolute path>
```
Example:
```shell script
stack exec hw2 -- "/Users/nikita/hw2-niki999922/hw2/testFolder/Root"
```

## Run Tests
```shell script
stack test
```
