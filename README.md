# Giraffe.Hopac

[Enter useful description for Giraffe.Hopac]

---

## Builds

MacOS/Linux | Windows
--- | ---
[![Travis Badge](https://travis-ci.org/TheAngryByrd/Giraffe.Hopac.svg?branch=master)](https://travis-ci.org/TheAngryByrd/Giraffe.Hopac) | [![Build status](https://ci.appveyor.com/api/projects/status/github/TheAngryByrd/Giraffe.Hopac?svg=true)](https://ci.appveyor.com/project/TheAngryByrd/Giraffe.Hopac)
[![Build History](https://buildstats.info/travisci/chart/TheAngryByrd/Giraffe.Hopac)](https://travis-ci.org/TheAngryByrd/Giraffe.Hopac/builds) | [![Build History](https://buildstats.info/appveyor/chart/TheAngryByrd/Giraffe.Hopac)](https://ci.appveyor.com/project/TheAngryByrd/Giraffe.Hopac)  


## Nuget 

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/Giraffe.Hopac)](https://www.nuget.org/packages/Giraffe.Hopac/) | [![NuGet Badge](https://buildstats.info/nuget/Giraffe.Hopac?includePreReleases=true)](https://www.nuget.org/packages/Giraffe.Hopac/)

---

### Building


Make sure the following **requirements** are installed in your system:

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

```
> build.cmd // on windows
$ ./build.sh  // on unix
```


### Watch Tests

The `WatchTests` target will use [dotnet-watch](https://github.com/aspnet/Docs/blob/master/aspnetcore/tutorials/dotnet-watch.md) to watch for changes in your lib or tests and re-run your tests on all `TargetFrameworks`

```
./build.sh WatchTests
```

### Releasing
* [Start a git repo with a remote](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/)

```
git add .
git commit -m "Scaffold"
git remote add origin origin https://github.com/user/MyCoolNewLib.git
git push -u origin master
```

* [Add your nuget API key to paket](https://fsprojects.github.io/Paket/paket-config.html#Adding-a-NuGet-API-key)

```
paket config add-token "https://www.nuget.org" 4003d786-cc37-4004-bfdf-c4f3e8ef9b3a
```


* Then update the `RELEASE_NOTES.md` with a new version, date, and release notes [ReleaseNotesHelper](https://fsharp.github.io/FAKE/apidocs/fake-releasenoteshelper.html)

```
#### 0.2.0 - 2017-04-20
* FEATURE: Does cool stuff!
* BUGFIX: Fixes that silly oversight
```

* You can then use the `Release` target.  This will:
    * make a commit bumping the version:  `Bump version to 0.2.0` and add the release notes to the commit
    * publish the package to nuget
    * push a git tag

```
./build.sh Release
```
