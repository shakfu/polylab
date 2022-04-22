

export const HelloWorld = () => <h1>Hello world</h1>;


class MyComponent {
  render() {}
}
// use a construct signature
const myComponent = new MyComponent();
// element class type => MyComponent
// element instance type => { render: () => void }
function MyFactoryFunction() {
  return {
    render: () => {},
  };
}
// use a call signature
const myComponent = MyFactoryFunction();
