---
title: Angular.js
published: January 1, 2014
excerpt: A framework for developing webapps
comments: off
toc: left
---

I've been meaning to learn Google's [Angular.js] framework for a while now. I of course have experience with straight up jQuery, but that inevitably becomes a mishmash of code. I do have some experience with [Backbone.js], which has been [battle tested] on many of websites such as Pandora, but something about angular seems a bit cleaner.

At the time of writing I am working on a web project in Go. I intend to make the Go backend serve an API which an Angular app will consume.

[Angular.js]: http://angularjs.org
[Backbone.js]: http://backbonejs.org/
[battle tested]: http://backbonejs.org/#examples

My main resources include the [tutorial] and a variety of other resources such as [egghead]'s videos.

[tutorial]: http://docs.angularjs.org/tutorial/step_00
[egghead]: https://egghead.io/lessons

* toc

# Bootstrapping

Application bootstrapping involves creating an injector used for dependency injection, the injector then creates a root scope to become the context of the application, and then angular compiles the DOM rooted at the `ngApp` root element, which processes directives and bindings.

*[DOM]: Document Object Model

# General Architecture

The `$scope` object allows explicit control over what models or parts of models are exposed to the view. The object is initialized in a controller, which is responsible for providing initial model values and defining functions. Models themselves can be any object.

## Scopes

Scopes inherit prototypically from each other. A variable defined in a higher-level scope is accessible to child-scopes. Writing to a variable in a child-scope doesn't propagate up the inheritance chain. Parent scopes can be directly referenced using the `$parent` property, but it's use is generally discouraged because it embeds an assumption about the overall DOM structure.

Because of the asymmetric behavior in reading/writing scope properties at different scopes, it's generally encouraged to bind to object properties, not scope properties. This has the effect of [always consulting the prototype chain][prototypes], since it first looks up the object, which it finds in its ancestor up the prototype chain, then it changes/sets the value on that object. Whereas _setting_ a single primitive (i.e. non-object) does _not_ consult the prototype chain and simply creates a new value in the child scope that shadows the parent value.

[prototypes]: https://github.com/angular/angular.js/wiki/Understanding-Scopes#javascript-prototypal-inheritance

``` html
<body ng-app ng-init="thing = {name: 'World'}">
<h1>Hello, {{thing.name}}</h1>
<div ng-controller="Ctrl">
  <input type="text" ng-model="thing.name">
  <h2>Hello, {{thing.name}}</h2>
</div>
</body>
```

Scopes form a tree rooted at the `$rootScope`, and events can be propagated through the hierarchy. Events are sent up the tree with `$emit` and down the tree with `$broadcast`. Scope event handlers are registered with `$on`, and as with DOM events, we can `preventDefault` and `stopPropagation`. It's advised to use scope events sparingly, instead opting for two-way data binding.

Scopes pose memory considerations, and are generally destroyed when they're no longer needed. This can be controlled explicitly with `$new` and `$destroy`.

# Services

It's possible to register object-creating recipes that are interpreted by the `$injector` to provide wired-up instances. Objects created by the `$injector` are called _services_, and these are singletons. Only one instance of any given service is created.

There are ways to control how objects are created. The first and simplest way is to register a pre-defined instance with `value`. Objects registered with `value` can't express dependencies.

``` javascript
var mod = angular.module('mod', []);
mod.value('someService', new SomeService());
```

The simplest way to register object recipes that can express dependencies is by registering the constructor function with `service`. This is generally useful for registering pre-existing constructor functions.

``` javascript
var SomeService = function(someDependency) {
  this.someVar = someDependency;
};

mod.service('someService', SomeService);
```

The `factory` method is more flexible than the previous two, since it can be used to register any arbitrary object-creating function. As a result of this flexibility, it's the most common way to register objects into the dependency injection system.

``` javascript
mod.factory('someService', function(someDependency) {
  var someVar = 0;

  return {
    method: function (arg) {
      someVar = arg;
    }
  };
});
```

Constants can be registered in order to be used by services. The problem with constants is that they have to be supplied as soon as a service expresses a dependency on it:

``` javascript
mod.factory('someService', function(someDependency, SOME_CONSTANT) {
  // ...
};)

mod.constant('SOME_CONSTANT', 10);
```

Providers is the generic way of registering services. A provider must return an object containing a `$get` property which is a factory function returning a instance of the service. Provider functions can have additional methods and properties, which is useful for configuration of the service.

``` javascript
mod.provider('someService', function () {
  var config = {someConfig: 10};
  var someVar = 0;

  return {
    setConfigVar = function(someConfig) {
      config.someConfig = someConfig || config.someConfig;
    },
    $get: function(someDependency) {
      return {
        someMethod: function (arg) {
          someVar = arg;
        };
      };
    }
  };
});
```

Providers can be configured before they produce any object instances. To facilitate this, module lifecycles consist of a _configuration phase_ where recipes are collected and configured, and a _run phase_ where it's possible to execute any post-instantiation logic.

The configuration phase allows providers to be configured as follows:

``` javascript
mod.config(function(someServiceProvider) {
  someServiceProvider.setConfigVar(20);
});
```

The run phase allows work to be done upon the application bootstrap, similar to the `main` entry function in programs, although Angular supports multiple run blocks.

``` javascript
mod.run(function($rootScope) {
  $rootScope.appStarted = new Date();
});
```

