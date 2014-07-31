---
title: React
published: July 25, 2014
excerpt: A novel library for building interfaces
comments: off
toc: left
---

[React] is a library for building user interface out of individual, composable components.

[React]: http://facebook.github.io/react/

* toc

# Components

Components are the unit for composing user interfaces in React and are essentially state machines. They can be viewed as functions that take in `props` and `state` and render HTML. Components can only render a single root node, so that multiple nodes must be wrapped in a single root node to be returned. The `renderComponent` function is used to render a React component into the DOM within the given container, known as _mounting_ in React. The `render` method should be pure.

``` javascript
var HelloMessage = React.createClass({
  render: function() {
    return <div>Hello {this.props.name}</div>;
  }
});

React.renderComponent(
  <HelloMessage name="John" />,
  document.getElementById('container')
);
```

State can be modified using the `setState(data, callback)` function which merges the `data` into the component's `state` and re-renders the component, calling the optional `callback` when it is finished rendering.

It's idiomatic to keep as many components stateless as possible. A common pattern is to create several stateless components under a stateful component which passes state to them via `props` so that the stateless sub-components can be rendered in a declarative manner.

State that may be contained in components should be the most primitive, absolutely required data that may be changed by event handlers to trigger a UI update. The `render` method can be used to compute more sophisticated data from this state automatically as required. State therefore shouldn't consist of computed data, other components (which should be constructed within `render`), or duplicate data from `props`.

_Reconciliation_ refers to the process by which React updates the DOM with each new render pass. Children are reconciled according to the order in which they're rendered, so that if a child node is removed, its contents are replaced with its sibling's contents and the sibling is destroyed. This may cause problems with stateful components, in which case it may be preferable to hide components instead of destroying them.

Components can be given a key that identifies them so that their state is kept with them across render passes by assigning the `key` property. Numerical identities should be preceded by a string to avoid being ordered separately from other properties. It's also possible to assign components to an object where each component is keyed by what will be its key.

Default properties can be defined with the `getDefaultProps` method so that if the parent component doesn't specify them they take on the default value.

``` javascript
var ComponentWithDefaultProps = React.createClass({
  getDefaultProps: function() {
    return {
      value: 'default value'
    };
  }
})
```

A React component that extends a basic HTML element can pass on its properties to the extended HTML element with `transferPropsTo`.

``` javascript
var Avatar = React.createClass({
  render: function() {
    return this.transferPropsTo(
      <img src={"/avatars/" + this.props.userId + ".png"} userId={null} />
    );
  }
});

// <Avatar userId={17} width={200} height={200} />
```

It's possible to specify validations for properties using `React.PropTypes`. For example, `React.PropTypes.component.isRequired` can be used to enforce that only one child is passed to a component as children.

``` javascript
var MyComponent = React.createClass({
  propTypes: {
    children: React.PropTypes.component.isRequired
  },
  render: function() {
    return <div>{this.props.children}</div>;
  }
})
```

## Mixins

Mixins can be used to share common functionality between components. If multiple mixins define the same lifecycle method, each one is guaranteed to be called.

``` javascript
var IntervalMixin = {
  componentWillMount: function() { this.intervals = []; },
  setInterval: function() {
    this.intervals.push(setInterval.apply(null, arguments));
  },
  componentWillUnmount: function() {
    this.intervals.map(clearInterval);
  }
};

var TickTock = React.createClass({
  mixins: [SetIntervalMixin],
  getInitialState: function() { return {seconds: 0}; },
  // call method from mixin
  componentDidMount: function() { this.setInterval(this.tick, 1000); },
  tick: function() { this.setState({seconds: this.state.seconds + 1}); },
  render: function() { return <p>Running for {this.state.seconds} seconds</p>; }
});
```

## Lifecycle

Components have three main stages in their lifecycle.

1. **Mounting**: insertion into the DOM
2. **Updating**: re-render to determine if DOM should be updated
3. **Unmounting**: removal from the DOM

There are three methods that can be implemented to hook into these stages before and after they occur.

### Mounting

The `getInitialState` method is invoked before a component is mounted to determine the initial state data. The `componentWillMount` method is invoked right before mounting. The `componentDidMount` method is invoked right after mounting, and should contain initialization that requires DOM nodes.

Once mounted, composite components support the `getDOMNode` method to obtain a reference to the DOM node, as well as the `forceUpdate` method to force an update when it's known that a deeper aspect of the state has changed without using `setState`.

### Updating

The `componentWillReceiveProps` method is called whenever a mounted component receives a new props object (passed through this method), and should be used to compare the existing props with the new props to perform any potential state transitions using `setState`.

The `shouldComponentUpdate` method is used to determine if any changes to the component warrant an update to the DOM. This should be used to compare the current and new state and props and return false to tell React that it doesn't need to update. This method receives the new set of props and state.

The `componentWillUpdate` method is called right before updating occurs and receives the new set of props and state, while the `componentDidUpdate` method is called right after updating completes and receives the old set of props and state.

### Unmounting

The `componentWillUnmount` method is called right before a component is unmounted and destroyed, so that cleanup may be handled.

## Event Handling

Event handlers can be established by using the HTML property in camel-case, such as `onClick`. All methods are automatically bound to their component instance.

Event handlers aren't attached to individual nodes. Instead, React establishes a top-level event listener that catches all events. When components are mounted or unmounted, an internal mapping is updated to reflect this.

# DOM

React maintains a fast in-memory DOM representation known as the virtual DOM. The `render` method returns a description of the DOM which can then be diffed with the virtual DOM to compute the fastest way to update the browser DOM to reflect any changes.

Sometimes it may be necessary to interact directly with the DOM, either to interoperate with third-party libraries or to perform non-reactive changes such as setting focus, which can't easily be inferred via React's data flow (props and state).

To interact with the actual browser DOM, a reference to a DOM node reference must be retrieved using the `getDOMNode` component method, which only works on mounted components. However, to be able to call this method on the component, it must be possible to get a reference to the component's backing instance.

Since what's returned by `render` are _not_ the rendered, _backing_ instances and instead are descriptions of the component's sub-hierarchy, it's not possible to obtain references to components in the following manner:

``` javascript
render: function() {
  var input = <input />;
  this.savedRef = input;
  return <div>{input}</div>;
}
```

Instead, components can be referenced by giving them a `ref` property which makes the component's backing instance accessible via `this.refs`. Once a reference to the component is attained, a DOM node reference can be retrieved with the `getDOMNode` method.

``` javascript
var MyComponent = React.createClass({
  handleClick: function() {
    this.refs.textInput.getDOMNode().focus();
  },
  render: function() {
    return (
      <div>
        <input type="text" ref="textInput" />
        <input type="button" value="click this" onClick={this.handleClick} />
      </div>
    );
  }
})
```

As a disclaimer, never access refs within any component's `render` method or while any component's `render` method is running anywhere in the call stack.

# Forms

Form components differ from native components because they can be mutated by user interactions. Specifically, they support props that are affected by user actions.

Prop       Components
-----      -----------
`value`    `input` and `textarea`
`checked`  `checkbox` and `radio`
`selected` `option`

Although in HTML the value of `textarea` is set via children, it should be set via `value` in React. The `onChange` prop can be listened to for when these properties change in response to user interactions.

If an `input` component has a `value` property set, then it is considered a _controlled component_, so that the rendered component _always_ reflects the `value` property; user input has no effect. In order for a controlled component to reflect user input, it should explicitly set the value in response to the change.

``` javascript
getInitialState: function() { return {value: 'Hello1'} },
handleChange: function(e) { this.setState({value: e.target.value}) },
render: function() {
  var value = this.state.value;
  return <input type="text" value={value} onChange={this.handleChange} />;
}
```

If an `input` component doesn't supply a `value` property, then it's _uncontrolled_ and so the rendered element reflects user input. A default value can be provided without making the component controlled by setting the `defaultValue` prop. There's also a `defaultChecked` that can be used with checkboxes.

``` javascript
render: function() {
  return <input type="text" defaultValue="Hello!" />;
}
```

# JSX

JSX is an optional HTML-like syntax that can be used for function calls that generate markup. A special comment header pragma is required to denote that the file should be processed by the JSX transformer.

``` javascript
/** @jsx React.DOM */
```

JSX can be used to construct instances of React DOM components and composite components created with `createClass`. It's important to realize that JSX has no notion of the DOM, and instead transforms elements into function calls.

``` javascript
var app = <Nav color="blue"><Profile>click</Profile></Nav>;
var app = Nav({color: "blue"}, Profile(null, "click"));

var MyComponent = React.createClass({/* ... */});
var app = <MyComponent someProperty={true} />;
```

JavaScript expressions---both attribute and child---can be embedded into JSX with curly braces `{}`. Comments should also be contained within expressions.

``` javascript
var person = <Person name={window.isLoggedIn ? window.name : ''} />;
var person = Person({name: window.isLoggedIn ? window.name : ''});

var content = <Container>{window.isLoggedIn ? <Nav /> : <Login />}</Container>;
var content = Container(null, window.isLoggedIn ? Nav(null) : Login(null));
```

Raw HTML can be inserted with a specific API.

``` javascript
<div dangerouslySetInnerHTML={{__html: 'First &middot; Second'}} />
```

# Flux

[Flux] is an architecture for apps where data flows in a unidirectional cycle. It consists of a dispatcher, stores, and views.

[Flux]: http://facebook.github.io/flux/

1. **Views**: perform actions---usually due to user interactions---which calls into the dispatcher with a data payload
3. **Dispatcher**: invoke callbacks registered by stores, sending action's data payload to interested stores
5. **Stores**: respond to actions they're interested in, receiving data payload, and emit "change" event
8. **Views**: listen to "change" events and re-render accordingly either implicitly or explicitly; **back to #1**

All data flows through the dispatcher which acts as a central hub. Actions are calls into the dispatcher and usually originate from user interactions with views. The dispatcher then invokes callbacks that the stores registered with it, thereby dispatching the data payloads contained within the actions to all stores. Within the registered callbacks, stores respond to the actions they're interested in and then emit a "change" event to alert controller-views to the fact that the data layer has been modified. The Controller-views listen to the change events and retrieve data from stores in an event handler, then call their own `rende` method via `setState` or `forceUpdate` to update themselves and all of their children.

## Dispatcher

The dispatcher is the central hub through which all data flows, acting as a registry of callbacks into the stores, such that each store registers itself and provides a callback. Stores can declaratively wait for other stores to finish updating and then update themselves accordingly.

The dispatcher exposes a method that allows a view to trigger a dispatch to the stores, with a data payload included.

The dispatcher can manage the dependencies between stores using the `waitFor` method which specifies a list of dispatcher registry indexes and a final callback to call after the callbacks at the given indexes have completed.

``` javascript
case 'TODO_CREATE':
  Dispatcher.waitFor([
    PrependedTextStore.dispatcherIndex,
    YetAnotherStore.dispatcherIndex
  ], function() {
    TodoStore.create(PrependedTextStore.getText() + ' ' + action.text);
    TodoStore.emit('change');
  });
  break;
```

## Stores

Stores contain the application state and logic, and are similar to models in MVCs. They receive the action's data payload as a parameter, which contains a type attribute identifying the action's type, so that a switch statement may be used to interpret the payload and provide proper hooks into the store's internal methods.

*[MVC]: Model-View-Controller

## Views and Controller-Views

Near the top of the nested view hierarchy there is a special kind of view that listens for events broadcast by the stores that it depends on. This can be called a controller-view since it provides the glue code to get the data from the stores and pass it down the chain of its descendants.

When the controller-view receives the event from the store, it requests the data it needs via the store's public getter methods, then calls its own `setState` and `forceUpdate` to re-render itself and its descendants. It's common to pass the entire state of the store down the chain of views so that each descendant can use what it needs.

