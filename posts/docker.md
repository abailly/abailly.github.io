---
title: Using Docker for Building
author: Arnaud Bailly 
date: 2013-11-25
---

* Goal: ensure repeatability and reliability of builds across OS (dev'ing on Windows, CI on linux, deploying on unix/solaris)
* Using docker inside a Linux VM on VirtualBox on windows
* Start from docker's github repo, use vagrant to generate VM with docker configured
* Create docker image from base (eg. ubuntu...)
* Install jdk, maven...
* Snapshot after first build => provides already loaded repository
* Beware of network connectivity. Had an issue while running tests, code was trying to connect to a server that was not reachable, HTTP client hanged in timeout which caused the test to fail as the test itself was timeouted

* Issue with repository update when offline => use host's local repository as remote

* probably not that useful to build java on linux => only execute tests!!
   * mount local repository
   * run mvn test:test
