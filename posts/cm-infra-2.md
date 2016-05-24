------------
title: Haskell-based Infrastructure
author: Arnaud Bailly 
date: 2016-05-24
------------

In my [previous post](/posts/cm-infra-1.html) I focused on the build and development tools. This post will conclude my series on Capital Match by focusing on the last stage of the rocket: How we build and manage our development and production infrastructure. As already emphasized in the previous post, I am not a systems engineer by trade, I simply needed to get up and running something while building our startup. Comments and feedback most welcomed!

# Continuous Integration

[Continuous Integration](http://www.extremeprogramming.org/rules/integrateoften.html) is a cornerstone of Agile Development practices and something I couldn't live without. CI is a prerequisite for *Continuous Deployment* or [Continuous Delivery](http://martinfowler.com/bliki/ContinuousDelivery.html): It should ensure each and every change in code  of our system is actually working and did not break anything. CI is traditionally implemented using servers like [Jenkins](https://jenkins.io/) or online services like [Travis](https://travis-ci.org/) that trigger a build each time code is pushed to a source control repository. But people like [David Gageot](http://blog.javabien.net/2009/12/01/serverless-ci-with-git/), among [others](http://www.yegor256.com/2014/10/08/continuous-integration-is-dead.html), have shown us that doing CI without a server was perfectly possible. The key point is that *it should not be possible to deploy something which has not been verified and validated by CI*.

We settled on using a central git repository and CI server, hosted on a dedicated build machine:

* Git repository's master branch is "morally" locked: Although technically it is still possible to push to it, we never do that and instead push to a `review` branch which is merged to the `master` only when build passes, 
* The git repository is configured with a git deploy hook](https://www.digitalocean.com/community/tutorials/how-to-use-git-hooks-to-automate-development-and-deployment-tasks) that triggers a call to the CI server when we push on the `review` branch,
* Our CI server is implemented with [bake](https://github.com/ndmitchell/bake), a robust and simple CI engine built - guess what? - in Haskell. Bake has a client/server architecture where the server is responsible for orchestrating builds that are run by registered clients, which are supposed to represent different build environments or configurations. Bake has a very simple web interface that looks like

    ![](/images/bake-screenshot.png)

* Bake provides the framework for executing "tests", reporting their results and merging changes to `master` branch upon successful build, but does not tell you *how* your software is built: This is something we describe in Haskell as a set of steps (bake calls them all *tests*) that are linked through dependencies and possibly dependent on the capabilities of the client. Here is a fragment of the code for building Capital Match:

    ```haskell
    data Action = Cleanup
                | Compile
                | Dependencies
                | RunDocker
                | Deploy ImageId
                | IntegrationTest
                | UITest
                | EndToEndTest
                deriving (Show,Read)

    allTests :: [Action]
    allTests = [ Compile
               , Dependencies
               , IntegrationTest
               , UITest
               , EndToEndTest
               , Deploy appImage
               , RunDocker
               ]

    execute :: Action -> TestInfo Action
    execute Compile = depend [Dependencies] $ run $ do
      opt <- addPath ["."] []
      () <- cmd opt "./build.sh --report=buildreport.json"
      Exit _ <- cmd opt "cat buildreport.json"
      sleep 1
      incrementalDone

    execute IntegrationTest = depend [Compile] $ run $ do
      opt <- addPath ["."] []
      () <- cmd opt "./build.sh test"
      incrementalDone
    ```

    The code is pretty straightforward and relies on the toplevel build script `build.sh` which is actually a simple wrapper for running our Shake build with various targets.
* The output of the CI process, when it succeeds, is made of a bunch of docker containers deployed to Dockerhub, each tagged with the SHA1 of the commit that succeeded,
* We extended bake to use [git notes](https://git-scm.com/docs/git-notes) to identify successful builds: We attach a simple note saying `Build successful` to those commits which actually pass all the tests. We also notify outcome of the build in our main [Slack](https://slack.com/) channel,
* Bake server and client are packaged and deployed as docker containers, which means we can pull and use those containers from any docker-enabled machine in order to reproduce a CI environment or trigger builds through bake's command-line interface,
* As the last stage of a successful build we deploy a test environment, using anonymized and redacted sample of production data.

# Deployment

## Provisioning & Infrastructure

We are using [DigitalOcean](https://www.digitalocean.com/)'s cloud as our infrastructure provided: DO provides a much simpler deployment and billing model than what provides AWS at the expense of some loss of flexibility. They also provide a simple and consistent RESTful API which makes it very easy to automate provisioning and manage VMs.

* I wrote a Haskell client for DO called [hdo](https://github.com/capital-match/hdo) which covers the basics of DO API: CRUD operations on VMs and listing keys,
* Provisioning is not automated as we do not need capacity adjustments on the go: When we need a machine we simply run the script with appropriate credentials. Having a simple way to provision VMs however has a nice side-effect: It makes it a no-brainer to fire copy of any environment we use (Dev, Ci or Production) and configure it. This was particularly useful for pairing sessions and staging deployment of sensitive features,
* We also use AWS for a couple of services: S3 to backup data and host our static web site and CloudFront to provide HTTPS endpoint to website.

## Configuration Management

Configuration of provisioned hosts is managed by [propellor](http://propellor.branchable.com/), a nice and very actively developed Haskell tool. Configuration in propellor are written as Haskell code using a specialized "declarative" embedded DSL describing *properties* of the target machine. Propellor's model is the following:

* Configuration code is tied to a git repository, which may be only local or shared, 
* When running `./propellor some.host`, it automatically builds then commits local changes, pushing them to remote repository if one is defined. All commits are expected to be signed,
* Then propellor connects through SSH to `some.host` and tries to clone itself there, either by plain cloning from local code if `some.host` has never been configured, or by merging missing commits if host has already been configured (this implies there is a copy of git repository containing configuration code on each machine),
* In case architectures are different, propellor needs to compile itself on the target host, which might imply installing additional software (e.g. a Haskell compiler and needed libraries...),
* Finally, it runs remote binary which triggers verification and enforcement of the various "properties" defined for this host.

Propellor manages security, e.g. storing and deploying authentication tokens, passwords, ssh keys..., in a way that seems quite clever to me: It maintains a "store" containing sensitive data *inside* its git repository, encrypted with the public keys of accredited "users", alongside a keyring containing those keys. This store can thus be hosted in a public repository, it is decrypted only upon deployment and decryption requires the deployer to provide her key's password.

Here is an example configuration fragment. Each statement separated by `&` is a property that propellor will try to validate. In practice this means that some system-level code is run to check if the property is set and if not, to set it.

```haskell
ciHost :: Property HasInfo
ciHost = propertyList "creating Continuous Integration server configuration" $ props
              & setDefaultLocale en_us_UTF_8
              & ntpWithTimezone "Asia/Singapore"
              & Git.installed
              & installLatestDocker
              & dockerComposeInstalled
```

In practice, we did the following:

* All known hosts configurations are defined in a configuration file (a simple text file containing a Haskell data structure that can be `Read`) and tells, for each known IP/hostname, what type of configuration should be deployed there and for production hosts what is the **tag** for containers to  be deployed there. As this information is versioned and committed upon each deployment run, we always know which version of the system is deployed on which machine by looking at this configuration,
* We also defined a special *clone*  configuration which allows us to deploy some version of the system using cloned data from another system,
* We ensure the application is part of the boot of the underlying VM: Early on we had some surprises when our provider decided to reboot the VM and we found our application was not available anymore...

## Deployment to Production

Given all the components of the application are containerized the main thing we need to configure on production hosts apart from basic user information and firewall rules is docker itself. Apart from docker, we also configure our  [nginx](http://nginx.org) frontend: The executable itself is a container but the configuration is more dynamic and is part of the hosts deployment. In retrospect, we could probably make use of pre-canned configurations deployed as data-only containers and set the remaining bits as environment variables.

Doing actual deployment of a new version of the system involves the following steps, all part of propellor configuration:

* We first check or create our data containers: Those are the containers which will be linked with the services containers and will host the persisted event streams (see [post on architecture](/posts/cm-archi.html)),
* We then do a full backup of the data, just in case something goes wrong...
* And finally rely on [docker-compose](https://docs.docker.com/v1.8/compose/) to start all the containers. The `docker-compose.yml` configuration file is actually generated by propellor from some high-level description of the system which is stored in our hosts configuration file: We define for each deployable service the needed version (docker repository *tag*) and use knowledge of the required topology of services dependencies to generate the needed docker links, ports and names.

The net result is the something like the following. The dark boxes represent services/processes while the lighter grayed boxes represent containers:

![](/images/services-architecture.png)

We were lucky enough to be able to start our system with few constraints which means we did not have to go through the complexity of setting up a blue/green or rolling deployment and we can live with deploying everything on a single machine, thus alleviating to use more sophisticated container orchestration tools.

### Rollbacks

[Remember](/posts/cm-archi.html) our data is a simple persistent stream of events? This has some interesting consequences in case we need to rollback a deployment:

* If the version number has not been incremented, rollbacking simply means reverting the containers' tag to previous value and redeploying: Even if some events have been recorded before we are notified of an issue implying rollback is needed, they should be correctly interpreted by the system,
* If the version has changed during deployment, then either we cannot rollback because new events have been generated and stored and we must roll-forward ; or we can rollback at the expense of losing data. This is usually not an option but still is possible if stored events are "harmless" business-wise, like authentication events (logins/logouts): A user will simply have to login again. 

## Monitoring

Monitoring is one the few areas in Capital Match system where we cheated on Haskell: I fell in love with [riemann](http://riemann.io) and chose to use it to centralize log collections and monitoring of our system.

* Riemann is packaged as a couple of containers: One for the server and one for the [dashboard](http://riemann.io/dashboard.html), and deployed on a dedicated (small) VM. Both server and dashboard configuration are managed by propellor and versioned,
* As part of the deployment of the various VMs, we setup and configure [stunnel](https://www.stunnel.org/index.html)s containers which allow encrypted traffic between monitored hosts and monitoring server: On the monitoring host there is a stunnel server that redirects inbound connections to running docker containers, whereas on monitored hosts the stunnel server is referenced by clients and encapsulate traffic to remote monitoring host transparently,
* Riemann is fed 2 types of events: 
    * System level events which are produced by a [collectd](https://collectd.org/) installed on each deployed host,
    * Applicative level events which are produced by the deployed services as part of our logging system,
* Applicative events are quite simple at the moment, mostly up/down status and a couple of metrics on HTTP requests and disk storage latency and throughput,
* There is a simple riemann dashboard that presents those collected events in a synoptic way,
* There a couple of alerts configured in Riemann that notifies *slack* when disks fill up or hosts are down.

We also have set up external web monitoring of both application and web site using [Check My Website](https://checkmy.ws/fr/).

# Discussion

