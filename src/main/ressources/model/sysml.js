(function() {  
  function makeDraggable(g,layout,options) {
    g.addEventListener("mousedown",function(start) {
      let sdx = layout.x;
      let sdy = layout.y;
      let handler = function (e) {
        layout.x = Math.max(0,sdx + e.clientX - start.clientX);
        layout.y = Math.max(0,sdy + e.clientY - start.clientY);
      }
      let end = function (e) {
        handler(e);
        window.removeEventListener("mousemove", handler);
        window.removeEventListener("mouseup", end);
        window.removeEventListener("mouseleave", end);
      }
      window.addEventListener("mousemove", handler);
      window.addEventListener("mouseup", end);
      window.addEventListener("mouseleave", end);
    });
  }

  function copy(obj) { return JSON.parse(JSON.stringify(obj)) }

  function getLayout(name, type, options) {
    let layout = {
      x: 0,
      y: 0,
      width: 0,
      height: 0,
      minWidth: 100,
      minHeight: 0
    }
    if (options.layout && options.layout.defaults && typeof options.layout.defaults[type] == "object") {
      for (var key in options.layout.defaults[type]) {
        if (options.layout.defaults[type].hasOwnProperty(key)) {
          layout[key] = options.layout.defaults[type][key];
        }
      }
    }
    if (options.layout && typeof (options.layout[name]) == "object") {
      for (var key in options.layout[name]) {
        if (options.layout[name].hasOwnProperty(key)) {
          layout[key] = options.layout[name][key] || layout[key];
        }
      }
    }
    let store = copy(layout);
    let listeners = { "layout": [] };
    function prop(name, events) {
      events.forEach(function(name) {
        if (!listeners[name]) listeners[name] = [];
      })
      return {
        get: function() {
          return store[name];
        },
        set: function(val) {
          if (typeof val == "number") 
            store[name] = Math.round(val);
          else 
            store[name] = val;        
          listeners.layout.forEach(function(listener) {
            listener(layout);
          });
          events.forEach(function(name) {
            listeners[name].forEach(function(listener) {
              listener(layout);
            });
          });
        }
      }
    }
    Object.defineProperties(layout,{
      x: prop("x",["translation", "x"]),
      y: prop("y",["translation", "y"]),
      width: {
        get: function() { return Math.max(store.width, store.minWidth) },
        set: prop("width",["size", "width"]).set
      },
      minWidth: prop("minWidth",["size", "width"]),
      height: {
        get: function() { return Math.max(store.height, store.minHeight) },
        set: prop("height",["size", "height"]).set
      },
      minHeight: prop("minHeight",["size", "height"]),
    });
    layout.bind = function(prop, listener) {
      setTimeout(function() {listener(layout)},0);
      listeners[prop].push(listener)
    };
    layout.unbind = function(prop, listener) {
      let index = listeners[prop].indexOf(listener);
      if (index >= 0) {
        listeners[prop].splice(index,1);
        return true;
      } else return false;
    }
    let anchors = {
      top: [],
      left: [],
      right: [],
      bottom: []
    }
    layout.bindAnchor = function(side, listener, bottomRight) {
      if (bottomRight) anchors[side].unshift(listener);
      else  anchors[side].push(listener);
    };
    layout.unbindAnchor = function(side, listener) {
      let index = anchors[side].indexOf(listener);
      if (index >= 0) {
        anchors[side].splice(index,1);
        return true;
      } else return false;
    }
    return layout;
  }

  function getAnchor(layout, anchor) {
    if (anchor == null) return {
      x: Math.round(layout.x + layout.width / 2),
      y: Math.round(layout.y + layout.height / 2)
    }
    if (typeof anchor == "string") {
      switch (anchor) {
        case "top": return {
          x: Math.round(layout.x + layout.width / 2),
          y: layout.y
        }
        case "bottom": return {
          x: Math.round(layout.x + layout.width / 2),
          y: layout.y + layout.height
        }
        case "left": return {
          x: layout.x,
          y: Math.round(layout.y + layout.height / 2)
        }
        case "right": return {
          x: layout.x + layout.width,
          y: Math.round(layout.y + layout.height / 2)
        }
        default: return {
          x: Math.round(layout.x + layout.width / 2),
          y: Math.round(layout.y + layout.height / 2)
        }
      }
    } else if (typeof anchor == "object") {
      let side = anchor[0]
      let pos = anchor[1]
      switch (side) {
        case "top": return {
          x: Math.round(layout.x + layout.width * pos),
          y: layout.y
        }
        case "bottom": return {
          x: Math.round(layout.x + layout.width * pos),
          y: layout.y + layout.height
        }
        case "left": return {
          x: layout.x,
          y: Math.round(layout.y + layout.height * pos)
        }
        case "right": return {
          x: layout.x + layout.width,
          y: Math.round(layout.y + layout.height * pos)
        }
        default: return {
          x: Math.round(layout.x + layout.width * side),
          y: Math.round(layout.y + layout.height * pos)
        }
      }
    } else {
      return {
        x: Math.round(layout.x + layout.width / 2),
        y: Math.round(layout.y + layout.height / 2)
      }
    }
  }

  let renderers = {    
    diagram: function (svg, diagram, options) {
      let frame = document.createElementNS("http://www.w3.org/2000/svg", "rect");    
      frame.setAttribute("x","0.5");
      frame.setAttribute("y","0.5");
      frame.classList.add("frame");
      svg.appendChild(frame);

      let title = document.createElementNS("http://www.w3.org/2000/svg", "text");
      let titleBorder = document.createElementNS("http://www.w3.org/2000/svg", "path");
      svg.appendChild(titleBorder);    
      title.classList.add("title");      
      diagram.title.forEach(function (part,i) {
        let span = document.createElementNS("http://www.w3.org/2000/svg","tspan");
        span.setAttribute("dominant-baseline","middle")
        span.textContent = part;
        title.appendChild(span);
        if (i == 0) {
          span.setAttribute("font-weight", "bold");
        } else {
          span.setAttribute("dx","8");
        }
      })    
      
      
      svg.appendChild(title);      
      let w = title.getBBox().width;
      let h = title.getBBox().height;
      title.setAttribute("x", 2 * options.spacing);
      title.setAttribute("y", options.spacing + h / 2);

      titleBorder.setAttribute("d",`M 0,${h + 2 * options.spacing} h${w + 2 * options.spacing} l${2 * options.spacing},-${2 * options.spacing} v-${h}`);

      if (options.width) frame.setAttribute("width", options.width);
      if (options.height) frame.setAttribute("height", options.height);

      let g = document.createElementNS("http://www.w3.org/2000/svg","g");    
      g.setAttribute("transform",`translate(${options.spacing * 2},${options.spacing * 4 + h})`);
      svg.appendChild(g);

      var rights = [];
      var bottoms = [];
      var i = 0;

      function updateFrame() {
        let right = Math.max.apply(null, rights);
        let bottom = Math.max.apply(null, bottoms);
        let w = title.getBBox().width;
        let h = title.getBBox().height;
        svg.setAttribute("viewBox",`0 0 ${right + options.spacing * 5} ${bottom + options.spacing * 6 + h}`)
        frame.setAttribute("width", right + 4 * options.spacing - 1);
        frame.setAttribute("height", bottom + 6 * options.spacing + h - 1);        
        title.setAttribute("y", options.spacing + h / 2);        
        titleBorder.setAttribute("d",`M 0,${h + 2 * options.spacing} h${w + 2 * options.spacing} l${2 * options.spacing},-${2 * options.spacing} v-${h}`);  
      }

      for (let block in diagram.blocks) {
        if (diagram.blocks.hasOwnProperty(block)) {
          let element = diagram.blocks[block];
          element.name = element.name || block;
          element.svg = render("block",g,element,options);
          rights.push(0);
          bottoms.push(0);
          let j = i;
          element.layout.bind("layout", function(l) {          
            rights[j] = l.x + l.width;
            bottoms[j] = l.y + l.height;
            updateFrame();
          })
          i += 1;
        }
      }

      diagram.connectors.forEach(function(connector){
        connector.parent = diagram;
        render("connector",g,connector,options);
      })

      return svg;
    },

    connector: function (diagram, connector, options) {
      let g = document.createElementNS("http://www.w3.org/2000/svg","g");
      diagram.appendChild(g);      
      let from = connector.parent.blocks[connector.from];
      let to = connector.parent.blocks[connector.to];
      let path = document.createElementNS("http://www.w3.org/2000/svg","path");
      let layout = options.layout[connector.name] || options.layout[connector.from + "->" + connector.to] || [null,null];
      let anchor1 = layout[0];
      let anchor2 = layout[1];      
      path.classList.add(connector.type);
      g.appendChild(path);
      function updatePath() {
        let start = getAnchor(from.layout, anchor1);
        let end = getAnchor(to.layout, anchor2);
        let d = "M " + start.x + " " + start.y + " "
        let a1 = typeof anchor1 == "string" ? anchor1 : anchor1[0]
        let a2 = typeof anchor2 == "string" ? anchor2 : anchor2[0]
        
        if (a1 == "top" || a1 == "bottom") {
          if (a2 == "top" || a2 == "bottom") {
            d += "L " + start.x + " " + ((start.y + end.y) / 2) + " "
            d += "L " + end.x + " " + ((start.y + end.y) / 2) + " "
          } else {
            d += "L " + start.x + " " + end.y + " "
          }
        } else {
          if (a2 == "top" || a2 == "bottom") {
            d += "L " + end.x + " " + start.y + " "
          } else {
            d += "L " + ((start.x + end.x) / 2) + " " + start.y + " "
            d += "L " + ((start.x + end.x) / 2) + " " + end.y + " "
          }
        }
        d += "L " + end.x + " " + end.y;
        path.setAttribute("d", d);
      }
      from.layout.bind("layout", function(l) {
        updatePath()
      })
      to.layout.bind("layout", function(l) {
        updatePath()
      })
      function positionLabel(anchor,label) {
        switch(anchor) {
          case "top":
            label.setAttribute("transform",`translate(${options.spacing}, -${options.spacing})`)
            label.setAttribute("dominant-baseline","alphabetical")
            label.setAttribute("text-anchor","start")
            break;
          case "left":
            label.setAttribute("transform",`translate(-${options.spacing}, -${options.spacing})`)
            label.setAttribute("text-anchor","end")
            break;
          case "bottom":
            label.setAttribute("transform",`translate(${options.spacing}, ${options.spacing})`)
            label.setAttribute("dominant-baseline","mathematical")
            label.setAttribute("text-anchor","start")  
            break;
          case "right":
            label.setAttribute("transform",`translate(${options.spacing}, -${options.spacing})`)
            label.setAttribute("text-anchor","start")  
            break;
          default:
            break;
        }
      }      
      if (connector.labels && connector.labels.start) {
        let label = document.createElementNS("http://www.w3.org/2000/svg", "text");
        label.textContent = connector.labels.start;
        let anchorLabel = anchor1 == null ? "null" : (typeof anchor1 == "object" ? anchor1[0] : anchor1)
        positionLabel(anchorLabel,label)
        from.layout.bind("layout", function (l) {
          pos = getAnchor(l, anchor1);
          label.setAttribute("x", pos.x);
          label.setAttribute("y", pos.y);
        })
        g.appendChild(label);
      }
      if (connector.labels && connector.labels.end) {
        let label = document.createElementNS("http://www.w3.org/2000/svg", "text");
        label.textContent = connector.labels.end;
        let anchorLabel = anchor2 == null ? "null" : (typeof anchor2 == "object" ? anchor2[0] : anchor2)
        positionLabel(anchorLabel,label)
        to.layout.bind("layout", function (l) {
          pos = getAnchor(l, anchor2);
          label.setAttribute("x", pos.x);
          label.setAttribute("y", pos.y);
        })
        g.appendChild(label);
      }
    },

    block: function (diagram, block, options) {
      let g = document.createElementNS("http://www.w3.org/2000/svg", "g");
      g.classList.add("block");

      let layout = getLayout(block.name, "block", options);
      block.layout = layout;

      layout.bind("translation", function(l) {
        g.setAttribute("transform", "translate(" + layout.x + "," + layout.y + ")");
      });

      makeDraggable(g, layout, options);
      diagram.appendChild(g);

      let frame = document.createElementNS("http://www.w3.org/2000/svg", "rect");
      g.appendChild(frame);
      frame.classList.add("frame");
      layout.bind("width", function(l) {
        frame.setAttribute("width", Math.max(l.width, l.minWidth));
      })
      layout.bind("height", function(l) {
        frame.setAttribute("height", Math.max(l.height, l.minHeight));
      })

      layout.height = options.spacing;

      if (block.stereotype) {
        let stype = document.createElementNS("http://www.w3.org/2000/svg", "text");
        g.appendChild(stype);        
        stype.textContent = "«" + block.stereotype + "»";
        stype.setAttribute("dominant-baseline","mathematical")
        stype.setAttribute("font-style","italic")
        stype.setAttribute("font-size", options.smallFont)
        stype.setAttribute("text-anchor","middle")
        let stypeHeight = stype.getBBox().height;
        stype.setAttribute("y", options.spacing);
        layout.height += stypeHeight;
        layout.bind("width",function(l) {
          stype.setAttribute("x", Math.max(l.width, l.minWidth) / 2);
        })
      }

      let title = document.createElementNS("http://www.w3.org/2000/svg", "text");
      g.appendChild(title);
      title.setAttribute("text-anchor", "middle")
      title.setAttribute("dominant-baseline","mathematical")
      title.setAttribute("font-weight","bold")
      title.textContent = block.name;
      title.setAttribute("y", layout.height);
      layout.height += title.getBBox().height;
      layout.bind("width",function(l) {
        title.setAttribute("x", Math.max(l.width, l.minWidth) / 2);
      })      

      for (let compartment in block.compartments) {
        if (block.compartments.hasOwnProperty(compartment)) {
          let separator = document.createElementNS("http://www.w3.org/2000/svg", "line");
          g.appendChild(separator);
          separator.setAttribute("x1", "0");
          separator.setAttribute("y1", layout.height);
          layout.bind("width", function(l) {
            separator.setAttribute("x2",Math.max(l.width,l.minWidth));
          })
          //separator.setAttribute("x2", frame.getAttribute("width"));
          separator.setAttribute("y2", layout.height);
          let container = document.createElementNS("http://www.w3.org/2000/svg", "g");
          container.setAttribute("transform", "translate(0," + layout.height + ")");
          g.appendChild(container);
          let element = block.compartments[compartment];
          let c = render("compartment", container, {
            name: compartment,
            items: element,
            parent: block
          }, options);
          layout.height += c.getBBox().height + options.spacing;
        }
      }

      for (var port in block.ports) {
        if (block.ports.hasOwnProperty(port)) {
          let type = block.ports[port];
          let g = document.createElementNS("http://www.w3.org/2000/svg", "g");
          diagram.appendChild(g);
          let frame = document.createElementNS("http://www.w3.org/2000/svg", "rect");
          g.appendChild(frame);
          frame.setAttribute("width", options.spacing * 2);
          frame.setAttribute("height", options.spacing * 2);
          let path = document.createElementNS("http://www.w3.org/2000/svg", "path");
          g.appendChild(path);
          path.setAttribute("d", "M 5,3 L 11,8 L 5,13");        
          let label = document.createElementNS("http://www.w3.org/2000/svg", "text");          
          g.appendChild(label);        
          label.textContent = port;
          if (type == "in") {
            label.setAttribute("x", -  options.spacing);
            label.setAttribute("y", options.spacing);
            layout.bind("layout", function (l) {
              let x = l.x - options.spacing
              let y = l.y + l.height / 2 - options.spacing
              g.setAttribute("transform", "translate(" + x + ", " + y + ")")
            });
          } else if (type == "out") {
            label.setAttribute("x", 3 * options.spacing);
            label.setAttribute("y", options.spacing);
            layout.bind("layout", function (l) {
              let x = l.x + l.width - options.spacing
              let y = l.y + l.height / 2 - options.spacing
              g.setAttribute("transform", "translate(" + x + ", " + y + ")")
            });
          }
        }
      }

      frame.setAttribute("height", layout.height);
      return g;
    },

    compartment: function (block, compartment, options) {
      let g = document.createElementNS("http://www.w3.org/2000/svg", "g");
      g.classList.add("compartment");
      block.appendChild(g);

      let title = document.createElementNS("http://www.w3.org/2000/svg", "text");
      title.textContent = compartment.name;
      title.setAttribute("font-style","italic")
      title.setAttribute("text-anchor","middle")
      title.setAttribute("dominant-baseline","mathematical")
      title.setAttribute("font-size",options.smallFont) 
      g.appendChild(title);
      let titleHeight = title.getBBox().height;
      title.setAttribute("y",options.spacing)
      compartment.parent.layout.bind("width", function(l) {
        title.setAttribute("x", Math.max(l.width,l.minWidth) / 2);
      });

      let top = 2 * options.spacing + titleHeight;
      compartment.items.forEach( (item) => {
        item.split("\n").forEach( (line) => {
          let container = document.createElementNS("http://www.w3.org/2000/svg", "g");
          container.setAttribute("transform", "translate(0," + top +")");
          g.appendChild(container);
          let text = document.createElementNS("http://www.w3.org/2000/svg", "text");
          container.appendChild(text);
          text.textContent = line;
          let indent = line.search(/\S|$/) * options.spacing
          text.setAttribute("x", options.spacing + indent);          
          let textHeight = text.getBBox().height;
          let textWidth = text.getBBox().width;
          compartment.parent.layout.minWidth = Math.max(compartment.parent.layout.minWidth,  textWidth + indent + options.spacing * 2);
          //text.setAttribute("y", textHeight);
          top += textHeight;
        })
      })

      return g;
    }
  }

  function render(type, parent, structure, options) {
    return renderers[type](parent, structure, options)
  }

  function addSaveButton() {  
    let button = document.createElementNS("http://www.w3.org/2000/svg","circle");
    button.classList.add("save-button");
    button.setAttribute("cx",32);
    button.setAttribute("cy",32);
    button.setAttribute("r",16);
    document.rootElement.appendChild(button);
    button.onclick = function () {
      document.rootElement.removeChild(button);
      document.removeChild(document.getElementById("script"));
    }
  }

  function init() {
    let svg = document.querySelector("svg")
    svg.setAttribute("font-size",options.normalFont) 
    render("diagram",svg,diagram,options);
  }

  document.addEventListener("DOMContentLoaded",init,{once: true})  
})();

function makeStatic() {
  let svg = document.querySelector("svg")
  let clone = svg.cloneNode(true)
  let iterator = document.createNodeIterator(clone,Node.ELEMENT_NODE)
  let currentNode
  while (currentNode = iterator.nextNode()) {    
    if (currentNode.tagName == "script") {
      currentNode.parentElement.removeChild(currentNode)
    }
  }
  let container = document.createElement("div")
  container.appendChild(clone)  
  let link = document.createElement("a")
  link.href = "data:image/svg+xml," + encodeURIComponent(container.innerHTML)
  link.download = "sysml.svg"
  link.click()
}