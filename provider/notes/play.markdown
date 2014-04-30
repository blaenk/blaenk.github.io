---
title: Play
published: April 7, 2014
excerpt: MVC web framework for Scala
comments: off
toc: left
---

So I've [learned] Scala the language, and now I think it's time to learn [Play], a popular web framework for it. There is also a browsable [API documentation].

[learned]: /notes/scala/
[Play]: http://www.playframework.com/
[API documentation]: http://www.playframework.com/documentation/2.0.x/api/scala/index.html#package

* toc

# Command

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

# Configuration

The `conf/application.conf`{.path} file is the master configuration file in Play. From there all aspects of Play and other third-party libraries like Akka can be configured. The configuration format is based on the [config] library which supports a superset of JSON, Java Properties, comments, file includes, file merging, units (e.g. days, MB, etc.), and so on.

[config]: https://github.com/typesafehub/config

*[JSON]: JavaScript Object Notation

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

## Routing

The `conf/routes`{.path} file contains the mapping of routes to controller actions.

```
GET /         controllers.Products.home()

# if parameter not passed, assume to be 1
GET /products controllers.Products.list(page: Int ?= 1)

# parameter is optional; may or may not appear
GET /apples   controllers.Apples.show(page: Option[Int])

# parameter is fixed to 1
GET /oranges  controllers.Oranges.show(page: Int = 1)

# route variable
GET /product/:ean controllers.Products.details(ean: Long)

# path parameter
GET /photo/*filepath controllers.Media.photo(file: String)

# path parameter with different external prefix
GET /assets/*filepath controllers.Assets.at(path="/public", file)

GET /product/$ean<\d{13}> controllers.Products.details(ean: Long)
```

Reverse routing is a means of programmatically generating routes for a given action method invocation.

``` scala
def delete(ean: Long) = Action {
  Product.delete(ean)
  // or url with routes.Products.list().url
  Redirect(routes.Products.list())
}
```

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

## Persistence

Play allows for _evolutions_ which are similar to Rails database migrations, which are stored in `conf/evolutions/default/`{.path} and are named `#.sql` where `#` is the revision number. Play automatically asks to apply the evolution the next time the application is accessed. Evolutions take the following form:

``` sql
# --- !Ups

CREATE TABLE products (id long, ean long);

# --- !Downs

DROP TABLE IF EXISTS products;
```

Play comes with Squeryl, a DSL for generating SQL in a type-safe manner, and Anorm, which allows raw SQL queries to be written explicitly.

<div class="callout">

**Note**: Recent versions of Play allow the use of [Slick], which is the preferred method of interfacing with databases going forward.

[Slick]: http://slick.typesafe.com/

</div>

Anorm has three ways of processing results: Stream API, pattern matching, and parser combinators. SQL queries are constructed using the `SQL` class. The `apply` method of `SQL` accepts an implicit parameter of type `java.sql.Connection`, which `Play` provides from `DB.withConnection`. This `apply` method returns a `Stream[SqlRow]` which can be `map`ped over.

The SQL query contains `SqlRow` objects which have an `apply` method that retrieves the field by name. An explicit type parameter is provided to ensure that the field gets cast to the correct Scala type.

``` scala
import play.api.Play.current
import play.api.db.DB

import anorm.SQL
import anorm.SqlQuery

object Product {
  def getAll: List[Product] = DB.withConnection { implicit connection =>
    val sql: SqlQuery = SQL("select * from products order by name asc")

    sql().map ( row =>
      Product(row[Long]("id"), row[Long]("ean"),
              row[String]("name"), row[String]("description"))
    ).toList
  }
}
```

The above transformation can also be applied with pattern matching.

``` scala
def getAll: List[Product] = DB.withConnection { implicit connection =>
  import anorm.Row

  val sql: SqlQuery = SQL("select * from products order by name asc")

  sql().collect {
    case Row(Some(id: Long), Some(ean: Long),
             Some(name: String), Some(description: String)) =>
         Product(id, ean, name, description)
  }.toList
}
```

Finally, the query can also be transformed with parser combinators.

``` scala
import anorm.RowParser

val productParser: RowParser[Product] = {
  import anorm.~
  import anorm.SqlParser._

  long("id") ~
  long("ean") ~
  str("name") ~
  str("description") map {
    case id ~ ean ~ name ~ description =>
      Product(id, ean, name, description)
  }
}
```

This creates a `RowParser` but Anorm expects a `ResultSetParser`. The `*` means to try to match zero or more rows.

``` scala
import anorm.ResultSetParser

val productsParser: ResultSetParser[List[Product]] = {
  productParser *
}
```

The `ResultSetParser` can then be passed to the query with the `as` method:

``` scala
def getAllWithParser: List[Product] = DB.withConnection {
  implicit connection =>

  sql.as(productsParser)
}
```

A multirecord parser can be written to construct instances of objects that themselves contain other class instances from a different table. We start out by writing a `StockItem` parser.

``` scala
val stockItemParser: RowParser[StockItem] = {
  import anorm.SqlParser._
  import anorm.~

  long("id") ~ long("product_id") ~
  long("warehouse_id") ~ long("quantity") map {
    case id ~ productId ~ warehouseId ~ quantity =>
      StockItem(id, productId, warehouseId, quantity)
  }
}
```

Now a parser is needed which can parse the combination of product and stock item. The SQL join query will return a list of single arrays containing the joined parts, `[Product, StockItem]`. The `flatten` method transforms each of the single arrays into a two element tuple.

``` scala
def productStockItemParser: RowParser[(Product, StockItem)] = {
  import anorm.SqlParser._
  import anorm.~

  productParser ~ StockItem.stockItemParser map (flatten)
}
```

Finally we can construct the method that will retrieve each of the values and transform them into a map keyed by the `Product` and containing `StockItem` values. This transformation is possible by using the `groupBy` function to group the results by the first tuple element, the `Product`. This still leaves the values containing `Product`, specifically a `Map[Product,List[(Product,StockItem)]]`, which can be removed by using `mapValues` and mapping the tuples to replace themselves with the second tuple element, the `StockItem` values, leaving a `Map[Product,List[StockItem]]`.

``` scala
def getAllProductsWithStockItems: Map[Product, List[StockItem]] = {
  DB.withConnection { implicit connection =>
    val sql = SQL("select p.*, s.*" +
                  "from products p" +
                  "inner join stock_items s on (p.id = s.product_id)")

    val results: List[(Product, StockItem)] = sql.as(productStockItemParser *)

    results.groupBy { _._1 }.mapValues { _.map { _._2 } }
  }
}
```

It's also possible to insert, update, and delete with Anorm. This is generally accomplished by preparing statements and calling `executeUpdate` on them.

``` scala
def insert(product: Product): Boolean =
  DB.withConnection { implicit connection =>
    val addedRows = SQL("""
    insert into products
    values ({id}, {ean}, {name}, {description})
    """).on(
      "id" -> product.id,
      "ean" -> product.ean,
      "name" -> product.name,
      "description" -> product.description
    ).executeUpdate()

    addedRows == 1
  }

def update(product: Product): Boolean =
  DB.withConnection { implicit connection =>
    val updatedRows = SQL("""
    update products
    set name = {name},
        ean = {ean},
        description = {description},
    where id = {id}
    """).on(
      "id" -> product.id,
      "name" -> product.name,
      "ean" -> product.ean,
      "description" -> product.description
    ).executeUpdate()

    updateRows == 1
  }

def delete(product: Product): Boolean =
  DB.withConnection { implicit connection =>
    val updatedRows = SQL("delete from products where id = {id}").
      on("id" -> product.id).
      executeUpdate()

    updatedRows == 0
  }
```

# Views

Templates in Play are type-safe and consist of interspersed Scala, avoiding the need to learn a template DSL. Templates can specify a parameter list as the first line, which can be used to provide data from the controller's end.

The `@` character is used to denote the start of a Scala expression, where the parser _conservatively_ determines the extent of the expression. Sometimes it may be necessary to explicitly define this range, which is possible by using parentheses. If it's necessary to print a `@` character verbatim, it must be escaped by another `@`. Comments are delimited with `@* *@`.

```
Hello @name!

@* output the year *@

Next year, your age will be @(user.age + 1)
```

The template parser encloses all template expressions in curly braces, so it's not possible to define a variable in one expression and use it in another in the same level. More specifically, the bodies of functions, for comprehensions, and so on are treated as "sub-templates," so the body isn't treated as XML literals in the case of HTML templates.

``` scala
@{var i = 0}
@articles.map { article =>
  @{i = i + 1} // yields not found: value i
  <li>@i - @article.name</li>
}

@for((article, index) <- articles.zipWithIndex) {
  <li>@(index + 1) - @article.name</li>
}
```

It's possible to clean up a lot of value declarations by defining them inside the for comprehension:

``` scala
@for((article, index) <- articles.zipWithIndex;
     rank = index + 1;
     first = index == 0;
     last = index == articles.length - 1) {
  // use values here
}
```

It's also possible to define a scope with an associated value, to avoid having to re-evaluate that expression each time.

``` scala
@defining(article.countReview()) { total =>
  <p>@total @total @total</p>
}
```

## Escaping

All Scala expressions' output is escaped. Values can be output unescaped by wrapping them in the `Html` type.

## Includes

There is no distinction between partials, known as _includes_ in Play, and regular templates. Any template can be embedded within another by simply calling the generated object's apply method in a Scala expression.

~~~ {.html text="partial.scala.html"}
@()
<strong>This is a partial</strong>
~~~

~~~ {.html text="index.scala.html"}
<div class="announcement">
  @navigation()
</div>
~~~

## Layouts

A layout can be created in a straightforward manner from the template concepts covered so far. First it's necessary to add a parameter of type `Html` to the layout template. Other templates that want to embed themselves in a layout simply call the layout template and use the braces `{}` method call syntax to pass the entire template to the layout.

~~~ {.html text="main.scala.html"}
@(title="Default Title")(content: Html)
<!DOCTYPE html>
<html>
  <head>
    <title>@title</title>
  </head>
<body>
  @content
</body>
</html>
~~~

~~~ {.html text="products.scala.html"}
@main("Products") {
  <strong>Some Products</strong>
}
~~~

It's possible to define implicit parameters on template parameter lists to avoid having to explicitly pass parameters to the templates. A common pattern is to define a reusable trait with the appropriate implicit values. The `WrappedRequest` class wraps a `Request` and can be extended by a class that defines other values that should be accessible by a template.

## Localization

Localization is pretty straightforward in Play. The `application.conf`{.path} file can take an `application.langs`{.path} option that defines a comma-separated list of languages in ISO 639-2 format optionally followed by an ISO 3166-1 alpha-2 country code.

```
application.langs="en,en-US,nl"
```

For each of these languages there should be a corresponding `conf/messages.lang`{.path} file which defines localized messages. These messages can then be referenced using the `Messages` object which takes the message's key as well as an implicit `Lang` value which can be implicitly converted from a `Request` in the scope.

Messages in the file are patterns formatted using [`java.text.MessageFormat`{.path}][messageformat], so that arguments can be inserted with `{#}` where the `#` is the position of the argument. It's also possible to define different messages for zero, one, more items:

[messageformat]: http://docs.oracle.com/javase/8/docs/api/java/text/MessageFormat.html

``` scala
// cart=Cart {0,choice,0#is empty|1#has one item|1< has {0} items}.

<p>@Messages("cart", 0)</p> // is empty
<p>@Messages("cart", 1)</p> // has one item
<p>@Messages("cart", 2)</p> // has 2 items
```

# Controllers

Controllers derive from `Controller` and are basically generators of actions by defining methods that return an instance of `Action`, which constructs a function that handles a request and returns a result. More specifically, this function is of type `Request[A] => Result` where `A` is the type of the request body.

They generally mark the `request` object as `implicit` so that it may be passed to functions implicitly.

``` scala
package controllers

import play.api.mvc.{Action, Controller}
import models.Product

object Products extends Controller {
  def list = Action { implicit request =>
    val products = Product.findAll
    Ok(views.html.products.list(products))
  }

  def details(ean: Long) = Action {
    NotImplemented // HTTP 501
  }
}
```

The `as` method allows setting the mime-type in a convenient manner, and the `withHeaders` method allows setting headers.

``` scala
Ok("some result")
  .as(JSON)
  .withHeaders(LOCATION -> url)
```

Actions can be composed, facilitating the re-use of common handlers such as authentication checking and caching:

``` scala
def list =
  Authenticate {
    Cached {
      Action {
        // process request
      }
    }
  }
```

## Session

The session is represented as a map `Map[String, String]` in `request.session` and can easily be modified with the `withSession` method.

``` scala
Ok(result).withSession(
  request.session + ("some.data" -> value)
)

// later on

val data = request.session.get("some.data")
```

A flash scope is available as in most other web frameworks which is essentially stored in the session and the values only live on until the next request. The `flashing` method makes this straightforward:

``` scala
Redirect(routes.Products.list()).flashing("info" -> "Product deleted!")

// later on

val message = request.flash("info")
```

## Assets

The assets controller allows for reverse routing to asset files.

``` scala
<link href="@routes.Assets.at("images/favicon.png")" type="image/png">
```

The assets controller handles this automatically by adding an `Etag` header. HTTP Entity Tags (ETags) allows a client to make a conditional HTTP request for a resource so that the server can tell it whether or not to use the cached copy instead.

Compression is also automatically enabled if:

* in production mode
* request is routed to assets controller
* HTTP request has `Accept-Encoding: gzip`
* file with same name and `.gz` suffix is found

Play has built-in support for Less and CoffeeScript. Such files can easily be referenced using the assets reverse router by its target extension. The extension can be prefixed by `min` to use a minified version. A file can opt-out of compilation by prefixing its name with an underscore.

# Validation

Play provides a forms API which is used for general validation, not just HTML forms. A `Mapping` is an object that constructs an object from an HTTP request, a process called _binding_, where the type of the object constructed is a type parameter of `Mapping`.

The data from an HTTP request is transformed into a `Map[String, String]`, and a mapping performs its construction off of this map. A mapping can also perform the reverse process, _unbinding_.

A mapping can also define constraints and errors to provide when data doesn't satisfy the constraints.

## Forms

[Predefined mappings] exist in the `Forms` namespace, such as `Forms.text`. Mappings can be composed together, for example using `Forms.tuple` which can bind values to a tuple type.

[Predefined mappings]: http://www.playframework.com/documentation/2.0/api/scala/play/api/data/Forms$.html

``` scala
val data = Map(
  "name"   -> "Box of paper clips",
  "ean"    -> "1234567890123",
  "pieces" -> "300"
)

val mapping: Mapping[(String, String, Int)] =
  Forms.tuple(
    "name"   -> Forms.text,
    "ean"    -> Forms.text,
    "pieces" -> Forms.number
)
```

To be able to bind data, it's necessary to wrap a mapping in a `Form`, which can also contain the data that will eventually be bound. The `Form`'s type parameter is the same as the `Mapping`'s, representing the data that would be available if it validates. Once the form is created, the data can be bound using the `bind` method. Since `Form` is immutable, the `bind` method returns a new `Form` populated with the bound data.

``` scala
val productForm = Form(mapping)
val processedForm = productForm.bind(data)
```

The form can be checked for errors with `hasErrors` and if there are any the errors can be fetched with `getErrors`, otherwise the data can be retrieved with `get`.

An alternative method of processing the result of binding the data is to use the `fold` method in the same manner that it'd be used on the `Either` type, where the first function is the error handler which is passed the form, and the second function is the success handler which is passed the bound data:

``` scala
processedForm.fold(
  formWithErrors => BadRequest,
  productTuple => {
    Ok(views.html.product.show(product))
  }
)
```
