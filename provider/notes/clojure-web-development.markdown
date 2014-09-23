---
title: Clojure Web Development
published: July 2, 2014
excerpt: Exploring the rich Clojure web development environment
comments: off
toc: left
---

I've noticed a lot of very interesting ideas coming out of the Clojure web development environment lately, so I decided to explore this space after [learning about Clojure](/notes/clojure/).

On the server-side, Clojure seems to favor basic libraries to build up applications instead of monolithic, opinionated web frameworks [^micro_frameworks]. The [Enlive] server-side templating library introduces a novel idea of transforming pure HTML for the purpose of dynamically generating content, instead of embedding a language within HTML as has been tradition.

[^micro_frameworks]: This is an increasingly popular approach with other languages as well, such as Python and Flask, or Go and its variety of web development libraries.

On the client-side, there's the venerable [ClojureScript] which I consider to be a more appealing JavaScript-based language than the existing ones (e.g. CoffeeScript). This has become a powerful technology to use alongside [Om], which provides a ClojureScript interface to [React], and leverages persistent, immutable datastructures for [remarkable performance increases].

[ClojureScript]: https://github.com/clojure/clojurescript/
[Om]: https://github.com/swannodette/om
[React]: http://facebook.github.io/react/
[remarkable performance increases]: http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/

* toc

# Architecture {#web-application-architecture}

A general architecture can be one where handlers, routes, models, and views are split off into their own namespaces. Routes relating to specific workflows would be defined in their own namespaces where that behavior is defined. The routes can then be combined into a single handler using Compojure's `routes` macro.

The `project.clj`{.path} file should contain the `:ring` declaration which defines the application handler and, optionally, functions to run on application startup and shutdown.

``` clojure
:ring {:handler guestbook.handler/app
       :init    guestbook.handler/init
       :destroy guestbook.handler/destroy}
```

# Handling

[Ring] is the most popular, lowest-level library for developing web applications. It consists of four components: handlers, requests, responses, and middleware.

[Ring]: https://github.com/ring-clojure/ring

Requests and responses are represented as standard Clojure maps. Handlers are functions that process incoming requests, taking request maps and returning response maps. The following response outputs the visitor's IP address.

``` clojure
(defn handler [request-map]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str "ip is " (:remote-addr request-map))})
```

Middleware can wrap handlers to modify the way the request is processed, or more specifically, a function that accepts an existing handler and returns a new handler with added behavior---a closure. The following adds a `Pragma` value to the response headers.

``` clojure
(defn wrap-nocache [handler]
  (fn [request]
    (let [response (handler request)]
      (assoc-in response [:headers "Pragma"] "no-cache"))))

(def app (wrap-nocache handler))
```

A decent middleware combination to use for a typical site is `compojure.handler/site`{.path}, which includes support for sessions, cookies, flash, and parameter destructuring.

The [lib-noir] library contains a variety of middleware, such as session and cookie handling, redirects, input validation, password hashing, and so on. Each middleware can wrap a previously defined application handler.

[lib-noir]: https://github.com/noir-clojure/lib-noir

Redirection is possible via `ring.util.response/redirect`{.path} which comes with Ring, however, lib-noir also has such a function as `noir.response/redirect`{.path}, which accepts an optional status code to supply. Both functions take a path---relative to the root---to which to redirect.

Session management is also provided by the `noir.session`{.path} namespace. The session store is handled by Ring, and a type of store must be specified, such as `ring.middleware.session.memory`{.path}, though there are other available back-ends for the session, such as Redis. The following session management functions are available:

Function    Description
---------   ------------
`clear!`    clear entire session
`flash-put` store flash value
`flash-get` get flash value
`get`       get session value
`put!`      set session value
`remove!`   remove session value

Input validation is provided by `noir.validation`{.path}. Validation is performed by specifying a set of rules for a handler using the `rule` function, which accepts a condition that must be satisfied, and a vector consisting of a field name as a keyword and an error to associate with it in the event that the condition is not satisfied. The `errors?` function is then used to see if there were any unsatisfied validations, in order to provide an alternative response, for example.

``` clojure
(defn handle-login [id pass]
  (rule (has-value? id)
    [:id "name required"])
  (rule (= id "foo")
    [:id "name must be 'foo'"])
  
  (if (errors? :id)
    (error-response)
    (normal-response)))
```

The `on-error` function can be used to format the error messages, if there are any. It takes as first argument the field name as a keyword and as second argument a function which gets passed the errors, and it returns that function's result.

``` clojure
(defn login-page []
  (layout/common
    (on-error :id
      (fn [errors] (apply str errors)))))
```

Hashing is exposed via `noir.util.crypt`{.path}, particularly functions `compare` and `encrypt`.

There are helper functions for specifying content-type in `noir.response`{.path}. The `content-type` function can take a MIME type and the response. There is also a `json` function that can automatically serialize a data structure passed to it.

It's possible to specify access rules for a group of paths by using `noir.util.middleware/wrap-access-rules`{.path} to wrap the access rules around the application handler.

``` clojure
(defn user-access [req]
  (session/get :user))

(def app
  (-> (routes private-pages app-routes)
    (middleware/app-handler)
    (middleware/wrap-access-rules
      [{:uri "/admin/*" :rule admin-access
        :uri "/user/*" :rules {:any [user-access admin-access]}}])))
```

A route should be wrapped with the `restricted` macro to express that it should be subject to access rules. The `def-restricted-routes` macro can be used to implicitly wrap a set of routes with `restricted` and give them a name.

``` clojure
(def-restricted-routes private-pages
  (GET "/profile" [] (show-profile))
  (GET "/my-secret-page" [] (show-secret-page)))
```

# Routing

Routes can be defined using [Compojure], effectively allowing handler functions to be associated with a URL and HTTP method.

A typical route declaration takes the HTTP method name (or `ANY` to match any method), the URI pattern to match, the list of parameters to pass on to the handler function, and the expression to use as the response. The `routes` function can be used to group multiple routes together into a single handler. In fact, the `defroutes` macro can also be used to generate a named handler.

[Compojure]: https://github.com/weavejester/compojure

``` clojure
(defn foo-handler [] "foo called")
(defn bar-handler [id] (str "bar called with id " id))

(def handler
  (routes
    (GET "/foo" [] (foo-handler))
    (GET "/bar/:id" [id] (bar-handler id))))

; or
(defroutes handler
  (GET "/foo" [] (foo-handler))
  (GET "/bar/:id" [id] (bar-handler id)))
```

The `context` macro can be used to specify routes with a common prefix, with the ability to capture any possible variables expressed in the prefix.

``` clojure
; routes for
; /user/:id/profile
; /user/:id/settings
; /user/:id/change-password

(def user-routes
  (context "/usr/:id" [id]
    (GET "/profile" [] (display-profile id))
    (GET "/settings" [] (display-settings id))
    (GET "/change-password" [] (change-password-page id))))
```

A symbol can instead be provided as the second argument in a route to bind the request map to that symbol within the context of the handler, as well as a destructured form to only bind certain keys.

``` clojure
((PUT "/:id" req (str "Requested " (:uri req))) {:uri "/foo"})
((PUT "/:id" {:keys [uri]} (str "Requested " uri)))
```

Form parameters are also accessible by name in the parameter list. It's also possible to bind a subset of the parameters and bind the rest to a map, or bind the whole parameter to a map.

``` clojure
(POST "/" [name message] (save-message name message))
(POST "/" [x y & z] ...)   ; z is map containing remaining parameters
(POST "/" [x y :as r] ...) ; r is complete request map
```

There are special route matchers for static resources and catch-all routes.

``` clojure
(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not found"))
```

# Resources

The [Liberator] library can simplify the creation of RESTful services by providing macros for defining service resources.

[Liberator]: http://clojure-liberator.github.io/liberator/

The `resource` macro can be used to define an anonymous resource, whereas the `defresource` macro can define a named resource. Both evaluate to a Ring handler which can then be passed to a Compojure route definition.

The Compojure route must use `ANY` as its method pattern to delegate the match to the resource definition.

``` clojure
(defroutes home-routes
  (ANY "/" request
    (resource
      :handle-ok "Works"
      :available-media-types ["text/plain"])))

; or
(defresource home
  :handle-ok "Works"
  :available-media-types ["text/plain"])

(defroutes home-routes
  (ANY "/" request home))
```

Resource definitions consist of maps with keys that are either a decision, handler, action, or declaration. The values of these keys are constants or functions, where the functions take the _context_ as their single argument. The context is a map containing keys for the request, resource, and optionally the representation---the result of content negotiation.

Decisions determine how to handle a request Their keys end in a question mark `?`, and their values must evaluate to a boolean value. If a function is used, the following return values are legal:

1. a **boolean** indicating the result of the decision
2. a **map** which is interpreted as having returned `true` and merges the map into the response map
3. a **vector** with first element being the outcome boolean and second element being the map to merge into the response map

If a decision is unsatisfied (i.e. evaluates to false), the corresponding error code is returned. For example, if `:service-available?` evaluated to false, it would return HTTP 503 Service not available.

Another decision that can be used is `:method-allowed?` which determines whether the HTTP request method is allowed. There is also the `:allowed-methods` key which takes a vector of methods as keywords.

``` clojure
(defresource home
  :method-allowed?
  (fn [context]
    (= :get (get-in context [:request :request-method]))))

; or
(defresource home
  :allowed-methods [:get])
```

Handlers are keys prefixed by `:handle-`, followed by the [name of an HTTP response]. These are usually paired with decisions.

[name of an HTTP response]: http://clojure-liberator.github.io/liberator/doc/handlers.html

``` clojure
(defresource home
  :method-allowed? (request-method-in :get)
  :handle-method-not-allowed
  (fn [context]
    (str (get-in context [:request :request-method]) " is not allowed")))
```

Actions exist for HTTP request methods PUT, POST, and DELETE, and they have a question mark suffix `!` to denote that they are mutating the application's internal state. When an action occurs, a result can be returned using `:handle-created`.

Declarations indicate the resource's capabilities, such as `:available-media-types` which specifies the MIME types that the resource can be returned as, or `:etag` for caching.

The [Chesire] library is used for fast and easy JSON parsing and generating.

[Chesire]: https://github.com/dakrone/cheshire

# Templating

There are two popular solutions for templating in Clojure. One represents HTML elements as vectors and the other transforms pure HTML.

## Hiccup

[Hiccup] represents HTML elements as vectors with optional maps of attributes. The `html` macro is used to generate the resulting HTML string from the Hiccup vectors. Setting id and classes can be done on the keyword itself.

[Hiccup]: https://github.com/weavejester/hiccup
[Enlive]: https://github.com/cgrand/enlive

``` clojure
(html [:div {:id "hello", :class "content"}
        [:p "Hello world!"]])
;= <div id="hello" class="content"><p>Hello world!</p></div>

(html [:div#hello.content [:p "Hello world!"]])
;= <div id="hello" class="content"><p>Hello world!</p></div>
```

There are helper functions that can generate HTML elements, and these can always take an optional map of attributes.

``` clojure
(link-to {:align "left"} "http://google.com" "google")
;= [:a {:align "left", :href #<URI http://google.com>} ("google")]

(form-to [:post "/"]
  [:p "Name: " (text-field "name")]
  [:p "Message: " (text-area {:rows 10 :cols 40} "message")]
  (submit-button "comment"))
```

The `defhtml` macro can be used to implicitly generate HTML from its body, to avoid having to use the `html` macro for each individual element.

``` clojure
(defhtml page [& body]
  [:html
    [:head
      [:title "Welcome"]
      [:body body]]])
```

The `include-css` and `include-js` functions can be used to generate tags that include resources in an HTML head section, each accepting a variable number of resources.

``` clojure
(defn common [& content]
  (html5
    [:head
      (include-css "/css/mobile.css"
                   "/some/path.css")
      (include-js "/some/path.js"
                  "/other/path.js")]
    [:body content]))
```

## Enlive

[Enlive] on the other hand implements a novel idea of performing transformations on pure HTML in order to generate dynamic content, instead of developing a language and embedding it within a template.

Enlive manages this by defining a template source, which consists of pure HTML. It then provides a variety of selectors to query and transform the HTML, much like jQuery.

Selectors are generally expressed as vectors. The `sniptest` function can be used to test selectors. The `content` is a function that transforms the content of the matched elements to the provided value.

``` clojure
(h/sniptest "<h1>Lorem Ipsum</h1>"
  [:h1] (h/content "Hello Reader!"))
```

Selectors are usually crafted by just taking the CSS selector and prepending colons before every separate selector.

``` clojure
; CSS: div span.phone
[:div :span.phone]
```

The rule for nested vectors is that the outer-most vector denotes hierarchical chaining, and all others denote conjunction (AND). Disjunctions (OR) are supported by wrapping the selector within a set.

``` clojure
; chained selector
; CSS: div span.phone
(equivalent
  [:div [:span :.phone]]
  [:div :span.phone])

; conjunction
; CSS: div span.phone.mobile
(equivalent
  [:div [:span [:.phone :.mobile]]]
  [:div :span.phone.mobile])

; disjunction
; CSS: div#info span.phone, div#info span.email
(equivalent
  #{[:div#info :span.phone] [:div#info :span.email]}
  [:div#info #{:span.phone :span.email}]
  [:div#info [:span #{:.phone :.email}]]
```

There are predicates that can be used, such as `attr?`. Note that the nested vector is required to denote that the predicate is part of the `:a` selector, otherwise it would be interpreted as global `*`.

``` clojure
; CSS: a[class]
[[:a (attr? :class)]]
```

Creating a custom selector is as simple as leveraging the `pred` function, which takes a predicate on an element. The following is like `attr?` except that it checks to see if any attribute on the element matches the value.

``` clojure
(defn some-attr=
  [value]
  (pred (fn [node] (some #{value} (vals (:attrs node))))))

(sniptest "<a one=\"needle\" two=\"haystack\">abc</a>"
  [some-attr= "needle"] (set-attr :one "found"))
;= "<a one=\"found\" two=\"haystack\">abc</a>"
```

Transformations can be a function of one element returning one element or a collection of elements or `nil`, or `nil` itself. As a result, conditions like `when` can be used as transformation functions, since they return `nil` is the condition is not met.

The `clone-for` function can be used to iterate and generate multiple elements.

``` clojure
(sniptest "<ul><li></li></ul>"
  [:li] (clone-for [i (range 3 0 -1)]
          content (str i)))
;= "<ul><li>3</li><li>2</li><li>1</li></ul>"
```

Sometimes the template source may contain attributes to make the selection process easier and/or faster. These attributes can be removed by using `do->`, which applies transformations in sequence.

``` clojure
(sniptest "<ul><li id=\"foo\"></li></ul>"
  [:#foo] (do->
            (remove-attr :id)
            (clone-for [i (range 3 0 -1)]
              (content (str i)))))
;= "<ul><li>3</li><li>2</li><li>1</li></ul>"
```

The `defsnippet` form defines a function that loads HTML from a file that can be transformed. Snippets are the equivalent of partials in other frameworks. Its first argument is the name to give the function, the second is the path to the file relative to the classpath, and the third is the selector to treat as the root element---to which transformations are applied. It returns a sequence of maps representing the HTML elements. These can be added into regular templates using transformation functions like `append`.

A single HTML file may contain several snippets, each selecting its own relevant nodes.

``` clojure
(defsnippet footer "footer.html" [:.footer]
  [message]
  [:.footer] (content message))

(footer "hello")
;= ({:tag :div, :attrs {:class "footer"}, :content ("hello")}])
```

The `deftemplate` form works the same way, but a root cannot be specified. Instead of returning a sequence of maps like `defsnippet`, it returns a lazy sequence of strings containing HTML.

``` clojure
(deftemplate friends-list "friends.html"
  [username friends]
  [:.username] (content username)
  [:ul.friends :li] (clone-for [f friends]
                      (content f)))
```

# Databases

Being JVM-based, Clojure has access to the various Java database libraries such as JDBC, ORMs like Hibernate, and its own libraries like [Korma]---which implements an EDSL for database interaction.

*[JVM]: Java Virtual Machine
*[ORM]: Object Relational Mapping

## JDBC

The `clojure.java.jdbc`{.path} namespace provides a thin layer between Clojure and [JDBC]. A database "spec" is created that is essentially a map of configuration data to locate the JDBC driver and configure it and its connections. Alternatively, a JDBC data source instance can be used.

[JDBC]: http://en.wikipedia.org/wiki/Java_Database_Connectivity

``` clojure
; example sqlite database spec
(def db-spec
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "test.db"})

(def db-spec2
  {:datasource
    (doto (PGPoolingDataSource.)
      (.setServerName "localhost")
      (.setDatabaseName "maindb")
      (.setUser "user")
      (.setPassword "pass")
      (.setMaxConnections 10))})
```

The `with-connection` function opens a connection to the database, and any expressions within its scope are executed within context of that connection. For example, `create-table` can be used within the scope to create a database table using either keywords or strings for the table and column names.

``` clojure
(jdbc/with-connection db-spec
  (jdbc/create-table :authors
    [:id "integer primary key"]
    [:first_name "varchar"]
    [:last_name "varchar"]))
```

The `insert-record` function inserts a single record taken as a map. The `insert-records` function can insert a variable number of records into a database, yielding an equal number of corresponding indices for every inserted record. The `insert-rows` function can insert a variable number of rows given a vector of values corresponding to each column.

The `insert-values` function can be used to insert a partial record, which takes a vector of keywords representing the columns that will be inserted, and a vector of the respective values for those columns.

``` clojure
(jdbc/with-connection db-spec
  (jdbc/insert-records :authors
    {:first_name "Jackie" :last_name "Chan"}
    {:first_name "John" :last_name "Doe"}))
;= ({:last_insert_rowid() 1}
;=  {:last_insert_rowid() 2})
```

Records can be updated with `update-values` or `update-or-insert-values`, the latter inserts the row if it doesn't already exist.

``` clojure
(jdbc/with-connection db-spec
  (sql/update-values :users
    ["id=?" "foo"] ; where id is "foo"
    {:pass "bar"}) ; set pass to "bar"
  
  (sql/update-or-insert-values :users
    ["id=?" "test"]
    {:pass "bar"}))
```

Records can be deleted using the `delete-rows` function:

``` clojure
(jdbc/with-connection db-spec
  (sql/delete-rows :users ["id=?" "foo"]))
```

The `with-query-results` function can be used to fetch data, with the result being a lazy sequence which performs the fetching of data until it's actually necessary, so long as the source remains available. For this reason, it's common to use `doall` on the result to force the data to be fetched.

``` clojure
(jdbc/with-connection db-spec
  (jdbc/with-query-results res ["SELECT * FROM authors"]
    (doall res))
;= ({:id 1, :first_name "Jackie", :last_name "Chan"}
;=  {:id 2, :first_name "John", :last_name "Doe"})

  (jdbc/with-query-results res ["SELECT * FROM authors WHERE id = ?" 2]
    (doall res)))
;= ({:id 2, :first_name "John", :last_name "Doe"})
```

Transactions can be performed using the `transaction` form which performs its body within a transaction, aborting the transaction if an exception is thrown of a constraint is violated.

``` clojure
(jdbc/with-connection db-spec
  (jdbc/transaction
    (jdbc/delete-rows :authors ["id = ?" 1])
    (throw (Exception. "Abort transaction!"))))
;= ; Exception Abort transaction!
```

## Korma

[Korma] is a EDSL for relational databases, which handles generating SQL, connection pooling, and so on. The `defdb` form defines a connection for Korma to use, which does this by taking a database spec as with JDBC. The most recently defined database with `defdb` becomes the default connection for Korma for all queries unless otherwise specified. Korma also sets up a connection pool for the database.

[Korma]: http://sqlkorma.com/

Entities express to Korma the specifications of properties in database tables and the relationship between tables, similar to models in ActiveRecord.

``` clojure
(use '[korma db core])
(defdb korma-db db-spec)

(declare author)

(defentity country
  (pk :id)
  (has-many author))

(defentity author
  (pk :id)
  (table :author)
  (belongs-to country))
```

Queries can be performed using the `select` macro, which accepts a variety of functions used to build queries. The `with` function, for example, includes a relation in the result.

``` clojure
(select author
  (with country)
  (where (like :first_name "Ja%"))
  (order :last_name :asc)
  (limit 1)
  (offset 1))
```

A query can be wrapped in the `sql-only` function to only generate the SQL, in order to, for example, print it out.

Korma represents queries as Clojure maps, allowing them to easily be manipulated by using the `select*` function to generate the map used to represent the query, instead of performing the query outright. Refining functions like `order`, `limit`, and `offset` take these maps as parameters to further modify the query. The `exec` function can ultimately be used to execute a query given one of these intermediary maps.

``` clojure
(def query (-> (select* author)
             (fields :last_name :first_name)
             (last 5)))
;= {:group [],
;=  :from [{:table "author",
;= ...
```

This can be used to great effect, such as performing multiple queries each with a different offset, used to paginate query results, for example.

``` clojure
(def humans (-> (select* humans)
                (order :date_of_birth)))

(let [kings-of-germany (-> humans
                         (where {:country "Germany"
                                 :profession "King"}))]
  (doseq [start (range 0 100 10)
          k (select kings-of-germany
              (offset start)
              (limit 10))]
    ; process results
    ))
```

# ClojureScript

[ClojureScript] is a language that compiles to JavaScript. It allows for seamless interop with JavaScript using the same notation as Java interop in regular Clojure. Standard JavaScript functions are available in the `js` namespace.

``` clojure
(defn log [& items]
  (.log js/console (apply str items)))
```

Properties are accessed using `.-` prefix notation. Properties can be set with the `set!` function, just as with Java interop.

``` clojure
(let [canvas (.getElementById js/document "canvas")
      width  (.-width canvas)
      height (.-height canvas)]
  (log "width: " width
       "height: " height)
  (set! (.-width canvas) 42))
```

Macros must be referenced with the `:require-macros` keyword in the namespace declaration. The macros must be defined in a _regular_ Clojure file with a `.clj`{.path} extension, and can have the same name for file and namespace as the file which references it.

``` clojure
(ns my.app
  (:require-macros [app.macros :as m]))
```

The `#js` tagged literal is for producing JavaScript objects and arrays, depending on the provided data structure. The literal is shallow, so that nesting JSON or arrays must also carry the `#js` literal tag.

``` clojure
; both produce the same JSON
#js {:foo "bar"}
#js {"foo" "bar"}

; produces JavaScript array
#js [1 2 3]

; #js is shallow
; this object contains a persistent vector
#js {:foo [1 2 3]}

; this object contains an array
#js {:foo #js [1 2 3]}
```

The `js->clj` function can be used to convert JSON to a Clojure map. The option `:keywordize-keys` can be used to turn the keys into keywords.

``` clojure
(let [cljmap (js->clj :keywordize-keys true)]
  ;; whatever
  )
```

The `..` Clojure macro can be used to chain multiple properties, for example.

``` clojure
; would access obj.prop.value.text
(.. obj -prop -value -text)
```

Functions that need to be accessible from outside the ClojureScript must be "exported" to prevent the Google Closure compiler from changing the function name during the minification process.

``` clojure
(defn ^:export init []
  (something))
```

External libraries can also be affected by this. A separate file can contain functions and variables that should be ignored by the minification process. This file should then be specified in the configuration map.

``` clojure
:prod {:source-paths ["src-cljs"]
       :compiler
         {:optimizations :advanced
          :externs ["resources/externs.js"]
          :output-to "output/file.js"}}
```

The [Domina] library is a ClojueScript interface to the DOM manipulation facilities of the Google Closure library. The [cljs-ajax] library can be used for AJAX calls.

[Domina]: https://github.com/levand/domina
[cljs-ajax]: https://github.com/JulianBirch/cljs-ajax

## Building {#building-clojurescript}

The [lein-cljsbuild] plug-in for lein can automate the compilation of ClojureScript by defining---in the `project.clj`{.path} file---the namespaces to reference and the JavaScript files to output.

[lein-cljsbuild]: https://github.com/emezeske/lein-cljsbuild

``` clojure
:cljsbuild
  {:builds
    [{:source-paths ["src-cljs"]
      :compiler
        {:pretty-print false
         :output-to "output/file.js"}}]}
```

The `:builds` key can accept a map consisting of different application profiles to fine-tune the configuration based on the profile.

``` clojure
:cljsbuild
  {:builds
    {:dev {:source-paths ["src-cljs"]
           :compiler
             {:pretty-print true
              :output-to "output/file.js"}}
     :prod {:source-paths ["src-cljs"]
            :compiler
              {:optimizations :advanced
               :output-to "output/file.js"}}}}
```

When configured this way, the profile can be specified as the final argument to general `cljsbuild` commands to specify which profile to use.

With this configuration, it's possible to compile the specified ClojureScript files either one time, or automatically whenever the files are changed. Such commands are "namespaced" by the `cljsbuild` command. The `clean` command can clear previously generated files.

``` bash
# one-off
$ lein cljsbuild once

# automatically
$ lein cljsbuild auto
```

# Om

[Om] is a ClojureScript interface to [React] leveraging immutable data structures for increased speed. One thing to keep in mind is that Om uses an optimization that always renders on `requestAnimationFrame`, unlike React, and so the state has to be set even if it's not changed.

[React]: /notes/react/

Application state in Om is held in an `atom`, which is the only reference type in ClojureScript. The application state can be transitioned with the `transact!` function which takes a transition function that shouldn't rely on information obtained by dereferencing a cursor, `get-state`, `transact!`, or `update!`. Changing the value via `swap!` or `reset!` always triggers a re-render of any roots attached to it. Everything in the atom must be an associative data structure, either map or an indexed sequence such as a vector. No lists or lazy sequences should be inside this state.

As in React, components take props, which in Om are actually [cursors] into the application state. This is relevant because in Om, the entire application state is stored in an atom, but individual components generally don't care about the entire scope of the application data. Each component gets cursors at construction time and automatically re-render themselves when the value underneath the cursor changes.

[cursors]: /notes/clojure#zippers

During the render phase, cursors can be treated as their underlying value (e.g. map or vector), but outside of the render phase they need to explicitly be dereferenced to yield this underlying value.

It's possible to create sub-cursors **only during the render phase** using the `get` or `get-in` functions but if the underlying value is a primitive, then the primitive is returned and not a cursor.

The consequence of this is that it's not possible to create a component that depends on a single string, such as a text-input. A workaround for this would be to make the component depend on a _vector_ of the single string.

``` clojure
(def state (atom {:name ["Igor"]}))

(defn text-input [cursor _]
  (render [_] (dom/input #js {:value (first cursor)})))

(render [_]
  ; yield sub-cursor to vector containing the string
  (om/build text-input (:name app-cursor)))
```

Cursors can propagate changes back to the original atom using the `transact!` function, which is available during and outside of the render phase. During the render phase, the `transact!`ed changes aren't visible until the next render. Outside of the render phase, `deref` returns the current value and `value` returns the last rendered value.

Components can depend on multiple cursors by simply wrapping them in a map or vector.

``` clojure
(def state (atom {:courses [...], :classes [...], ...}))

(render [_]
  (om/build table-view {:rows (:courses state),
                        :cols (:classes state)}))
```

The `root` function is used for mounting a component on a specific element in the DOM, like `React.renderComponent`. It takes a function that returns an Om component conforming to the `IRender` interface (like the `component` macro generates when the owner doesn't need to be accessed) given the application state and the backing React component, the application state atom, and a map containing the `:target` DOM node and any other options.

``` clojure
(om/root
  (fn [app owner]
    (om/component (dom/h1 nil (:text app))))
  app-state
  {:target (. js/document (getElementById "app"))})
```

DOM elements take the same attributes as in React: attributes and a body.

``` clojure
(def app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

(om/root
  (fn [app owner]
    (apply dom/ul #js {:className "animals"}
      (map (fn [text] (dom/li nil text)) (:list app))))
  app-state
  {:target (. js/document (getElementById "app"))})
```

Om components have to be built using `build` for single components or `build-all`.

``` clojure
(defn contact-view [contact owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil (display-name contact)))))

(defn contacts-view [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (dom/h2 nil "Contact list")
        (apply dom/ul nil
          (om/build-all contact-view (:contacts app)))))))
```

Components can communicate using `core.async`{.path} channels. To use this it is recommended to use `IRenderState` instead of `IRender`, so that state can be passed to it as the component state.

``` clojure
(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
        (dom/span nil (display-name contact))
        (dom/button
          #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))
```

The encompassing component can implement `IInitState` in order to initialize the state, which in this case is simply a `core.async`{.path} channel. This implements `IRenderState` as well so that it can receive the state and pass it on to its children.

``` clojure
(defn contacts-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)})

    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/div nil
        (dom/h2 nil "Contact list")
        (apply dom/ul nil
          (om/build-all contact-view (:contacts app)
            {:init-state {:delete delete}}))))))
```

The protocol `IWillMount` is then implemented to establish a `go` loop that listens for events from the children contact views. The `get-state` function can be used to get a component's state. The `get-node` function can be used to get a reference to a component via a `ref`.

``` clojure
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
          (let [contact (<! delete)]
            (om/transact! app :contacts
              (fn [xs] (vec (remove #(= contact %) xs))))
            (recur))))))
```

# Deployment

Clojure web applications are generally packaged and deployed as servlets. Servlets are Java classes that extend the `javax.servlet.http.HttpServlet`{.path} base class, which itself defines an interface for handling HTTP requests. Servlets can be deployed to one of many application servers. Application servers usually provide multitenancy, so that multiple applications can be deployed to the same application server. Most application servers are also web servers, but it's possible to proxy to a dedicated web server as well.

A Clojure web application using Ring, for example, can produce a servlet wrapper at runtime and hand that to the application server that runs embedded within the same process.

``` clojure
(use '[ring.adapter.jetty :only (run-jetty))
(def server (run-jetty #'app {:port 8080 :join? false}))
```

It's also possible to deploy to a standalone application server, however, by packaging up the web application into a war file. The war files are a variation of jar files. They contain a `web.xml`{.path} file that describes how the war file should be deployed, a `lib/`{.path} directory containing the application's dependencies so as to make the war self-contained, and a `classes/`{.path} directory containing the Clojure source files, JVM class files, and other assets.

The `web.xml`{.path} file specifies the configuration for the deployment of the war file, including servlet mount points, behavior of user sessions, and app-server specific features.

Leiningen can build war files---as well as accompanying `web.xml`{.path} files---using plug-ins such as `lein-ring`. This plugin requires a `:ring :handler` slot which specifies the namespace-qualified name of the top-level application request handler.

``` clojure
(defproject com.some/project "1.0.0"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure/compojure "1.0.1"]
                 [ring/ring-servlet   "1.0.1"]]
  plugins [[lein-ring "0.6.2"]]
  :ring {:handler com.some.site/routes})
```

With this configuration, the war file could be created using the following command:

``` bash
$ lein ring uberwar
```

Applications can be run locally for development and testing using Jetty with the following command. On each request, the Clojure source files are reloaded using the `require` function [^play_reload].

[^play_reload]: This is similar to Play's development server.

``` bash
$ lein ring server
```

## HTTP Kit

[HTTP kit] is a highly-concurrent HTTP server for Clojure. It's a near drop-in replacement for Jetty. A `main` method must be created that is accessible from Java, and the namespace that defines it must ensure that it is AOT compiled with the `:gen-class` key in the `ns` form. It's also necessary to define the entry-point in the `project.clj`{.path} file.

[HTTP kit]: http://http-kit.org/

``` clojure
(ns app.main
  (:use app.handler
        [org.httpkit.server :only [run-server]])
  (:gen-class)) ; ensure this ns is compiled

(defn -main [& [port]]
  (let [port (if port (Integer/parseInt port) 3000)]
    (run-server app {:port port})
    (println (str "You can view the site at http://localhost:" port))))
```

Finally, an uberjar can be generated and the jar can be run like any other.

``` bash
$ lein uberjar
$ java -jar target/app-0.1.0-SNAPSHOT-standalone.jar
```

