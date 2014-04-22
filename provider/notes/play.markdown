---
title: Play
published: April 7, 2014
excerpt: MVC web framework for Scala
comments: off
toc: left
---

So I've [learned] Scala the language, and now I think it's time to learn [Play], a popular web framework for it.

[learned]: /notes/scala/
[Play]: http://www.playframework.com/

* toc

# Play Command

The `play` command is similar to the `rails` command. The `new` option bootstraps a simple application with the following directory structure:

Directory  Purpose
---------- --------
app        application source
conf       configuration files/data
project    project build scripts
public     static files
test       tests

The `run` option starts the Play server and runs the application. Specifying it as `~run` performs compilation as files are modified, whereas regular `run` recompiles upon the next HTTP request.

Running `play` without any options starts the Play console. The `console` command can then start a Scala console which provides access to the Play application.

# Models

A model tends to consist of a model class (definition and attributes), data access object (to access model data), and some test data. The model is typically defined as a case class and its data access object is represented by a companion object.

``` scala
package models

case class Product(
  ean: Long, name: String, description: String)

object Product {
  var products = Set(
    Product(1L, "House", "A huge house"),
    Product(2L, "Boat", "A tug boat"),
    Product(3L, "Car", "A luxurious car")
  )

  def findAll = products.toList.sortBy(_.ean)
}
```

# Views

Templates consist of a first line containing template parameters. Embedded Scala statements are prefixed with `@`.

``` scala
@(products: List[Product])(implicit lang: Lang)

@main(Messages("application.name")) {
  <dl class="products">
    @for(product <- products) {
      <dt>@product.name</dt>
      <dd>@product.description</dd>
    }
  </dl>
}
```

Tags are similar to partials in other templating systems and they essentially get compiled into functions. Their starting line, for this reason, is a parameter list.

``` scala
@(ean: Long)
<img class="barcode" alt="@ean" src="@routes.Barcodes.barcode(ean)">
```

# Controllers

Controllers derive from `Controller` and generally mark the `request` object as `implicit` so that it may be passed to other functions implicitly.

``` scala
package controllers

import play.api.mvc.{Action, Controller}
import models.Product

object Products extends Controller {
  def list = Action { implicit request =>
    val products = Product.findAll
    Ok(views.html.products.list(products))
  }
}
```

# Configuration

The **conf/application.conf** file is the master configuration file in Play. From there all aspects of Play and other third-party libraries like Akka can be configured. The configuration format is based on the [config] library which supports a superset of JSON, Java Properties, comments, file includes, file merging, units (e.g. days, MB, etc.), and so on.

[config]: https://github.com/typesafehub/config

*[JSON]: Javascript Object Notation

Environment variables are often used for operating system-independent, machine-specific configuration. The `${}` syntax is used to interpolate environment variables and other properties as well. These can be interpolated into other values.

```
db.default.url = ${DATABASE_URL}

log.directory = /var/log
log.access = ${log.directory}/access.log
log.error  = ${log.directory}/error.log
```

It's also possible to include other files, which allows to create context-specific configuration files that are then included into the larger configuration file. Objects can be merged together as well based on the order in which they appear.

~~~ {text="db.conf"}
db: {
  default: {
    driver: "org.h2.Driver",
    url: "jdbc:h2:mem:play",
    user: "sa",
    password: "",
  }
}
~~~

~~~ {text="application.conf"}
include "db.conf"

db.default.user = products
db.default.password = clippy
~~~

There's also a configuration API for type-safe access to configuration properties. Currently the supported types are `String`, `Int`, and `Boolean`, where `Boolean` can take on `true`, `yes`, and `enabled` (and their opposites).

``` scala
import play.api.Play.current

current.configuration.getString("db.default.url").map {
  databaseUrl => Logger.info(databaseUrl)
}
```

Since the configuration is structured hierarchically, it's possible to get sub-configuration values for a given level:

``` scala
current.configuration.getConfig("db.default").map {
  dbConfig =>
  dbConfig.getString("driver").map(Logger.info(_))
  dbConfig.getString("url").map(Logger.info(_))
}
```
