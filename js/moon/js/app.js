Moon.component("header", {
  // options
  props: ['content'],
  template: "<p>Header Component has a {{content}}</p>"
});

Moon.component("content", {
  // options
  template: "<p>Content Here!</p>"

});

Moon.component("counter-component", {
  template: `<div>
      <p>Count: {{count}}</p>
      <button m-on:click="increment">Increment</button>
    </div>`,
  data: function() {
    return {
      count: 0
    }
  },
  methods: {
    increment: function() {
      this.set("count", this.get("count") + 1);
    }
  }
});

Moon.component("counter-child-component", {
  template: `<div>
    <p>Count: {{count}}</p>
    <button m-on:click="increment">Increment</button>
  </div>`,
  data: function() {
    return {
      count: 0
    }
  },
  methods: {
    increment: function() {
      this.set("count", this.get("count") + 1);
      this.emit("increment");
    }
  }
});

Moon.component("footer", {
  // options
  template: "<p>Footer Component!</p>"

});

const app = new Moon({
  el: "#app",
  data: {
    total: 0,
    msg: "Hello Moon!"
  },
  methods: {
    incrementTotal: function() {
      this.set("total", this.get("total") + 1);
    }
  }
});
