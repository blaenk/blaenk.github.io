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

## Environments

It's straightforward to setup different configuration environments by using the `include` directive in configuration files. Production configuration files would need to use the `classpath` function to refer to files within the deployed archive. This can be used to define a configuration file at a system path such as `/etc/paperclips/production.conf`{.path}, which can then be imported with the `-Dconfig.file` parameter.

```
include classpath("application.conf")

email.override.enabled=false
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

**Note**: Recent versions of Play allow the use of [Slick], which is the preferred method of interfacing with databases going forward.

[Slick]: http://slick.typesafe.com/

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

# Forms

Play provides a forms API which is used for general validation, not just HTML forms. A `Mapping` is an object that constructs an object from an HTTP request, a process called _binding_, where the type of the object constructed is a type parameter of `Mapping`.

The data from an HTTP request is transformed into a `Map[String, String]`, and a mapping performs its construction off of this map. A mapping can also perform the reverse process, _unbinding_.

A mapping can also define constraints and errors to provide when data doesn't satisfy the constraints.

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

Aside from tuple mappings, it's also possible to construct objects from mappings. This is possible by using the `mapping` method which takes a map of mappings, a function used to construct the object, and one to deconstruct it. These last two functions are provided for free by case classes in the form of `apply` and `unapply`. The first takes a parameter for every field in the case class, whereas the second takes an object of that type and deconstructs it into an `Option` tuple containing each of the fields.

``` scala
import play.api.data.Forms._

case class Product(
  name: String,
  ean: String,
  pieces: Int)

val productMapping = mapping(
  "name"   -> text,
  "ean"    -> text,
  "pieces" -> number)(Product.apply)(Product.unapply)
)
```

Creating a mapping for a class allows the construction of a `Form` parameterized by that class, making it much more natural to handle forms.

``` scala
val productForm = Form(productMapping)

productForm.bind(data).fold(
  formWithErrors => ...,
  product => ...
)
```

So far forms have been found from maps, but yielding a map from an HTTP request isn't very straightforward. For this reason, the `bindFromRequest` form method exists which works the same way as `bind`.

``` scala
def processForm() = Action { implicit request =>
  productForm.bindFromRequest.fold(
    ...
  )
}
```

## View Helpers

There are helper methods for generating forms whose output can be customized. As in Rails, the helpers can take extra parameters of type `(Symbol, Any)` to specify attributes such as the `class`.

``` scala
@helper.form(action = routes.GeneratedForm.create) {
  @helper.inputText(productForm("name"))
  @helper.textarea(productForm("description"))
  @helper.checkbox(productForm("active"))

  <div class="form-actions">
    <button type="submit">Create Product</button>
  </div>
}
```

The action must then pass the form to the action so that it may be accessed by the helper:

``` scala
def createForm() = Action {
  Ok(views.html.products.form(productForm))
}
```

A custom input element is possible with the following:

``` scala
@helper.input(myForm("mydatetime")) { (id, name, value, args) =>
  <input type="datetime" name="@name"
    id="@id" value="@value" @toHtmlArgs(args)>
}
```

It's also possible to customize the HTML that's generated from a particular helper. This HTML is generated by a `FieldConstructor` which is an implicit parameter to helper methods. The `FieldConstructor` trait has a single `apply` method which takes a `FieldElements` object and returns `Html`. The `FieldElements` object contains information about the data that gets passed through to the HTML. The simplest way to use this is by creating a template that takes a `FieldElements` parameter.

``` scala
@(elements: views.html.helper.FieldElements)

@import play.api.i18n._
@import views.html.helper._

<div class="control-group @elements.args.get('_class)
            @if(elements.hasErrors) {error}"
     id="@elements.args.get('_id).getOrElse(elements.id + "_field")">
  <label class="control-label" for="@elements.id">
    @elements.label(elements.lang)
  </label>
  @elements.input
</div>
```

However, although this template takes a `FieldConstructor` and returns `Html`, it doesn't extend the `FieldConstructor` trait. This can be remedied simply by creating a wrapper and defining an `apply` method that calls the template.

``` scala
package object bootstrap {
  implicit val fieldConstructor = new FieldConstructor {
    def apply(elements: FieldElements) =
      bootstrap.bootstrapFieldConstructor(elements)
  }
}
```

## Validation

Although mappings are immutable, the `verifying` method takes `Constraint[T]*` which copies the mapping and adds the provided constraints. There are some predefined constraints:

``` scala
// maximum value for an Int mapping
max(maxValue: Int): Constraint[Int]

// maximum length for a String mapping
maxLength(length: Int): Constraint[String]

// e.g.
"name" -> text.verifying(Constraints.nonEmpty)
```

Custom validators can be created by using the `verifying` method on mappings, which accepts a function that gets passed the bound object and returns a boolean indicating whether the constraint is met. It's possible to pass a string as the first parameter, which serves as the custom validation message.

``` scala
def eanExists(ean: Long) = Product.findByEan(ean).isempty

"ean" -> longNumber.verifying(eanExists(_))
```

Since mappings can be composed of other mappings, we can put constraints on more than one constituent mapping by applying the `verifying` method at the outer level. However, the error message for the top-level mapping has no key. Instead, if the top-level mapping produces an error, it's called the _global error_ which can be retrieved with the `globalError` method on `Form`, which returns an `Option[Error]`.

## Optional Values

Optional form values can be represented with the `Option` type by using the `Forms.optional` method which transforms `Mapping[A]` into `Mapping[Option[A]]`.

``` scala
case class Person(name: String, age: Option[Int])

val personMapping = mapping(
  "name" -> nonEmptyText,
  "age" -> optional(number)
)(Person.apply)(Person.unapply)
```

## Repeated Values

It's also possible to repeat mappings to accept a list of values using the `list` mapping transformer, which transforms a mapping from `Mapping[A]` to `Mapping[List[A]]`. The `seq` transformer is similar, but using a `Seq` instead of a `List`.

``` scala
/*
<input type="text" name="tags[0]">
<input type="text" name="tags[1]">
<input type="text" name="tags[2]">
*/

"tags" -> list(text)
```

Repeated mappings like these can be displayed easily with form helpers by using the `repeat` helper.

``` scala
@helper.repeat(form("tags"), min = 3) { tagField =>
  @helper.inputText(tagField, '_label -> "Tag")
}
```

## Nested Mappings

Nested mappings can be referred to using dot-notation similar to object dot notation. One reason for nesting mappings, aside from organizational purposes, is to avoid reaching the hard limit of 18 tuple fields which are used to back mappings.

``` scala
val contactMapping = tuple(
  "name" -> text,
  "email" -> email
)

val contactsForm = Form(tuple(
  "main_contact" -> contactMapping,
  "technical_contact" -> contactMapping,
  "administrative_contact" -> contactMapping,
))

@helper.inputText(form("main_contact.name"))
@helper.inputText(form("main_contact.email"))
```

## Custom Mappings

There are two ways to construct a custom mapping. The first involves transforming from an existing one, and the second involves building one from scratch. Transforming an existing mapping can be done using the `transform` method, which accepts a function from the existing type to the target type, and another for the reverse since mappings are bidirectional. However, `transform`'s limitation is that it has no way of expressing failure.

``` scala
val localDateMapping = text.transform(
  (dateString: String) => LocalDate.parse(dateString),
  (localDate: LocalDate) => localDate.toString
)
```

The process of creating a custom mapping from scratch involves implementing the `Formatter` trait. The mapping can then be constructed using the `Forms.of` method.

``` scala
trait Formatter[T] {
  def bind(key: String, data: Map[String, String]):
    Either[Seq[FormError], T]

  def unbind(key: String, value: T): Map[String, String]

  val format: Option[(String, Seq[Any])] = None
}

implicit val localDateFormatter = new Formatter[LocalDate] {
  def bind(key: String, data: Map[String, String]) =
    data.get(key) map { value =>
      Try {
        Right(LocalDate.parse(value))
      } getOrElse Left(Seq(FormError(key, "error.date", Nil)))
    } getOrElse Left(Seq(FormError(key, "error.required", Nil)))

  def unbind(key: String, ld: LocalDate) = Map(key -> ld.toString)

  override val format = Some(("date.format", Nil))
}

val localDateMapping = Forms.of(localDateFormatter)
val localDateForm = Form(single(
  "introductionDate" -> localDateMapping
))
```

## File Uploads

File uploads require the `multipart/form-data` encoding on forms.

It can be handled using the `parse.multipartFormData` body parser.

``` scala
def upload() = Action(parse.multipartFormData) { implicit request =>
  val form = Form(tuple(
    "description" -> text,
    "image" -> ignored(request.body.file("image")).
                 verifying("File missing", _.isDefined)
  ))

  form.bindFromRequest.fold(
    formWithErrors => {
      Ok(views.html.fileupload.uploadform(formWithErrors))
    },
    value => {
      file.ref.moveTo(new File("/tmp/image"))
      Ok("Retrieved file %s" format file.filename)
    }
  )
}
```

The above handler would correspond to the following.

``` scala
@helper.form(action = routes.FileUpload.upload,
             'enctype -> "multipart/form-data") {
  @helper.inputText(form("description"))
  @helper.inputFile(form("image"))
}
```

# JSON

Play's `play.api.libs.json.Json`{.path} module contains support for JSON. Default types can be converted to JSON with the `toJson` method resulting in a `JsValue`, which can be converted to a string with the `stringify` method. The response methods such as `Ok` know about `JsValue` and automatically set the `Content-Type` header to `application/json`. The JSON types include:

* `JsString`
* `JsNumber`
* `JsBoolean`
* `JsObject`
* `JsArray`
* `JsNull`

Arbitrary objects can be converted to JSON using a JSON formatter. The way this works is that `toJson` takes a second, implicit parameter of type `Writes[T]` where `T` is the type being serialized. `Writes[T]` is a trait with a single method that takes an object of type `T` and returns a `JsValue`.

``` scala
import play.api.libs.json._

case class Product(ean: Long, name: String, description: String)

implicit object ProductWrites extends Writes[Product] {
  def writes(p: Product) = Json.obj(
    "ean" -> Json.toJson(p.ean),
    "name" -> Json.toJson(p.name),
    "description" -> Json.toJson(p.description)
  )
}

// alternatively
val adminProductWrites: Writes[Product] = (
  (JsPath \ "ean").write[Long] and
  (JsPath \ "name").write[String] and
  (JsPath \ "description").write[String] and
  (JsPath \ "price").write[BigDecimal] and
)(unlift(Product.unapply))
```

To go in the other direction, parsing an object from a JSON value, we use the `Reads` trait.

``` scala
implicit val productReads: Reads[Product] = (
  (JsPath \ "ean").read[Long] and
  (JsPath \ "name").read[String] and
  (JsPath \ "description").read[String] and
)(Product.apply _)

val parsedProduct = JsValue.as[Product]
```

There are a couple of query methods that can be used on JSON values. The `\` method selects an element. The `\\` method selects a property anywhere in the JSON tree. The `apply` method returns an element from a `JsArray`.

``` scala
val companyName = (json \ "company" \ "name").asOpt[String]
```

Since it's common to want to implement `Reads` and `Writes` to serialize and deserialize objects, the `Format[T]` exists that represents both.

``` scala
case class PricedProduct(
  name: String,
  description: Option[String],
  purchasePrice: BigDecimal,
  sellingPrice: BigDecimal
)

implicit val productFormat = (
  (JsPath \ "name").format[String] and
    (JsPath \ "description").formatNullable[String] and
    (JsPath \ "purchase_price").formatNullable[BigDecimal] and
    (JsPath \ "selling_price").formatNullable[BigDecimal]
)(PricedProduct.apply, unlift(PricedProduct.unapply))
```

Formatters can also be created at compile time.

``` scala
implicit val productReads = Json.reads[PricedProduct]
implicit val productWrites = Json.writes[PricedProduct]

// or
implicit val productFormat = Json.format[PricedProduct]
```

Constraints can be added to fields to perform validation. This is done by supplying a custom `Reads` implementation to use for the validation as an implicit parameter to `read` or `readNullable`. The JSON can then be validated using the `validate` method.

``` scala
def save = Actions(parse.json) { implicit request =>
  val json = request.body
  json.validate[Product].fold(
    valid = { product =>
      Product.save(product)
      Ok("Saved")
    },
    invalid = {
      errors => BadRequest(JsError.toFlatJson(errors))
    }
  )
}
```

# Authentication

One way to handle authentication is to create a wrapper around `Action` that serves an HTTP Unauthorized error code if authentication fails.

``` scala
def AuthenticatedAction(f: Request[AnyContent] => Result):
  Action[AnyContent] = {
  Action { request =>
    if (authenticate(request)) {
      f(request)
    } else {
      Unauthorized
    }
  }
}
```

# Modules

Modules can be added via sbt, which the `play` command is actually a wrapper of. Some modules may be found in repositories other than the default ones, in which case a _resolver_ must be added. The following shows how to add the [SecureSocial] module.

[SecureSocial]: http://securesocial.ws/

``` scala
import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {
  val appName = "product-details"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "securesocial" %% "securesocial" % "2.1.0"
  )

  val main = PlayProject(appName, appVersion,
    appDependencies, mainLang = SCALA
  ).settings(
    resolvers += Resolver.url("SecureSocial Repository",
      url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/")
    )(Resolver.ivyStylePatterns)
  )
}
```

Creating modules is also straightforward and begins by creating a new Play application and only keeping the necessary files. This means that if we can remove `app/public`{.path} and `app/views`{.path} if we don't need them. It's important to keep in mind naming collisions however, which is why it's useful to create packages out of source files with the `package` keyword in Scala.

# Deployment

Play has two commands that make it very easy to deploy. The `stage` command compiles the application to a JAR file together with all dependency JARs and places the file in `target/staged`{.path} along with script `target/start`{.path} which can be used to start the application. The `dist` command packages up start script and dependencies into a zip archive which can easily be transferred.

Packaging up the application in this manner allows it to be deployed on any target that contains a Java runtime installation.

# Web Services

The `WS` object makes it simple to consume web services. The `WS.url` method creates a `WSRequestHolder` object can be constructed in method-chaining style, ultimately followed by a call to the request type, i.e. `get`.

``` scala
def itemList() = Action {
  Async {
    val response: Future[Response] =
      WS.url("http://someservice.com/api.json")
        .withQueryString("q" -> query)
        .get

    response.map { response =>
      val items = Json.parse(response.body).\("results").as[Seq[Item]]
      Ok(views.html.items.itemList(items))
    }
  }
}
```

# Caching

There's a caching API represented by the `Cache` object that can be accessed by having an implicit `play.api.Application`{.path} in the scope, which can be fulfilled by importing `play.api.Play.current`{.path}.  The `getOrElse` method can get a value for a given key and if not found, compute it and store it with an optional expiration time.

``` scala
Cache.set("user-key", User("John Doe"))
val userOption: Option[User] = Cache.getAs[User]("user-key")
```

It's also possible to use the `Cached` object to wrap an `Action` that can take a key and optional expiration time.

``` scala
def handler() = Cached("handler", 120) {
  Action { ... }
}
```

It's also possible to cache dynamic content by supplying a function to `Cached` that determines the string key to use based on the `RequestHeader`.

# Iteratees

Iteratees are objects that received individual chunks of data and does something with them. They have two type parameters: the first indicating the type of chunks it accepts and the second indicates the type of the final result the iteratee produces. Consider an iteratee that logs every chunk using the `foreach` function which takes a chunk of type `A` and returns an `Iteratee[A, Unit]`. It can be used with a `WSRequestHolder` by passing a function of type `ResponseHeaders => Iteratee` to the request type.

``` scala
val loggingIteratee = Iteratee.foreach[Array[Byte]] { chunk =>
  val chunkString = new String(chunk "UTF-8")
  println(chunkString)
}

WS.url("https://stream.twitter.com/1/statuses/sample.json")
  .sign(OAuthCalculator(consumerKey, accessToken))
  .get(_ => loggingIteratee)
```

Alternatively we can create an iteratee that produces an end-result, such as summing up integer chunks. Enumerators are the counterpart to iteratees in that they are producers of chunks and can be applied to iteratees, yielding futures of the new iteratee. Iteratees are immutable, so the result is simply a new iteratee with a new state.

``` scala
val summingIteratee = Iteratee.fold(0) { (sum: Int, chunk: Int) =>
  sum + chunk
}

val intEnumerator = Enumerator(1, 2, 3, 4, 5)

val newIterateeFuture: Future[Iteratee[Int, Int]] =
  intEnumerator(summingIteratee)

val resultFuture: Future[Int] = newIterateeFuture.flatMap(_.run)
resultFuture.onComplete(sum => println("Sum is %d" format sum))
```

## WebSockets

WebSockets are created using a pair of an iteratee used to process the incoming data stream and an enumator used to send data to the client.

In a simple chat application, an Akka actor can be used to process users, specifically to keep track of all of the users in the room. A `Concurrent.broadcast` yields a pair of an enumerator and a channel tied to that enumerator which allows one to push additional data to it after it has been created. The actor's private state will therefore include a set of users and a broadcast channel and its associated enumerator.

``` scala
class ChatRoom extends Actor {
  val users = Set[String]()
  val (enumerator, channel) = Concurrent.broadcast[String]
```

The `receive` method will handle events where a user joins, leaves, or broadcasts a message. Joining is handled by first checking if that user exists, and if so, ignoring anything that user might say. Otherwise, a message is broadcast to the room announcing that the user joined and adding them to the set of user names taken. The `mapDone` method is used to specify the behavior to perform when the iteratee has finished sending data.

``` scala
  def receive = {
    case Join(nick) => {
      if (!users.contains(nick)) {
        val iteratee = Iteratee.foreach[String]{ message =>
          self ! Broadcast("%s: %s" format (nick, message))
        }.mapDone { _ =>
          self ! Leave(nick)
        }

        users += nick
        channel.push("User %s has joined the room, now %s users"
          format(nick, users.size))
        sender ! (iteratee, enumerator)
      } else {
        val enumerator = Enumerator(
          "Nickname %s is already in use." format nick)
        val iteratee = Iteratee.ignore
        sender ! (iteratee, enumerator)
      }
    }
```

The Akka actor can also handle the case where a user leaves by simply removing the username from the set of usernames and broadcasting the event to the room.

``` scala
    case Leave(nick) => {
      users -= nick
      channel.push("User %s has left the room, %s users left"
        format(nick, users.size))
    }
```

Finally, the broadcast event simply adds a message to the channel that was created with `Concurrent.broadcast`.

``` scala
    case Broadcast(msg: String) => channel.push(msg)
  }
}
```

This actor can then be used in a controller by creating an instance of the actor. The `showRoom` method will render the template showing the chat room. The `chatSocket` is an action of type `WebSocket.async` which accepts parameters of type `Future[(Iteratee, Enumerator)]`.

The `chatSocket` action sends the `Join` message to the actor using the `?` method which essentially expects a response, which in this case is a pair of iteratee and enumerator, with the iteratee being the stream used to communicate with the server and the enumerator being the broadcast stream that is sent to the user. Finally, the `mapTo` method is used to cast the `Future[Any]` to the appropriate type of `Future[(Iteratee, Enumerator)]`.

``` scala
object Chat extends Controller {
  implicit val timeout = Timeout(1 seconds)
  val room = Akka.system.actorOf(Props[ChatRoom])

  def showRoom(nick: String) = Action { implicit request =>
    Ok(views.html.chat.showRoom(nick))
  }

  def chatSocket(nick: String) = WebSocket.async { request =>
    val channelsFuture = room ? Join(nick)
    channelsFuture.mapTo[(Iteratee[String, _], Enumerator[String])]
  }
}
```

## Body Parsers

Normally actions are only invoked when the request is complete, once the body parser is done parsing the body of the request. Suppose a user is uploading a large file only to be rejected once the upload is complete because the file is of the wrong type. A body parser can respond before the request is complete.

A body parser is an object that can process an HTTP request body, such as the `json`  or `multipartFormData` body parsers. More specifically, a body parser is a function that takes a `RequestHeader` parameter and returns an iteratee `Iteratee[Array[Byte], Either[Result, A]]`, that is, it processes the body of type `Array[Byte]` and returns an `Either[Result, A]`, where an error `Result` is return in the event of failure to construct `A`.

Essentially, Play creates a `RequestHeader` which is used to route to the appropriate `Action` and the body parser is used to create an `Iteratee` that's fed the body of the request. When the body is finished constructing, it's used to construct a complete `Request` which is then fed to the `Action`. If the construction fails, a `Request` won't be constructed and instead the `Result` will be returned to the client.

Built-in body parsers allow a maximum body size to be specified, which defaults to 512 kilobytes, and if exceeded, yields a `EntityTooLarge` HTTP error response.

The `temporaryFile` body parser parses the body and put its in a temporary file, which can then be moved to a permanent location with the `moveTo` method if the file satisfies any constraints. The `Play.getFile` method can yield a path relative to the application root.

``` scala
def upload = Action(parse.temporaryFile) { request =>
  val destinationFile = Play.getFile("uploads/myfile")
  request.body.moveTo(destinationFile)
  Ok("File successfully uploaded!")
}
```

It's possible to compose body parsers. For example, the `when` method allows one to specify a predicate and a parser to use if it's satisfied, as well as a failure result if not.

``` scala
def fileWithContentType(filename: String, contentType: String) =
  parse.when(
    requestHeader => requestHeader.contentType == contentType,
    parse.file(Play.getFile(filename)),
    requestHeader => BadRequest(
      "Expected a '%s' content type, but fuond %s".
        format(contentType, requestHeader.contentType)))

def savePdf(filename: String) = Action(
  fileWithContentType(filename, "application/pdf")) { request =>
    Ok("Your file is saved!")
  }
)
```

Body parsers also support the `map` function to transform the body parser's constructed type. This has the advantage of constructing the desired type and making it available within the action.

