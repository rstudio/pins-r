HTMLWidgets.widget({
  name: 'nomnoml',
  type: 'output',

  factory: function(el, width, height) {
    var g = null;
    var svg = null;
    var canvas = null;
    var wrapper = null;
    var code = null;
    
    var resizeSvg = function(width, height) {
      el.style.width = width;
      el.style.height = height;
      
      wrapper.setAttribute('width', width);
      wrapper.setAttribute('height', height);
      
      var scale = Math.min(width / svg.getAttribute("width"), height / svg.getAttribute("height"));
      var innerWidth = svg.getAttribute("width") * scale;
      var innerHeight = svg.getAttribute("height") * scale;
      g.setAttribute(
        "transform",
        "translate(" + (width  / 2 - innerWidth / 2) +
        "," + (height / 2 - innerHeight / 2) + ")" +
        "scale(" + scale + ") "
      );  
    };
    
    var resizeCanvas = function(width, height) {
      nomnoml.draw(canvas, code);
      
      var actualWidth = canvas.getAttribute('width');
      var actualHeight = canvas.getAttribute('height');
      
      var scale = Math.min(width / actualWidth, height / actualHeight);
      var innerWidth = actualWidth * scale;
      var innerHeight = actualHeight * scale;
      
      var transformX = -actualWidth / 2 * (1 - scale);
      var transformY = -actualHeight / 2 * (1 - scale);
      
      transformY += height / 2 - actualHeight * scale / 2;
      transformX += width / 2 - actualWidth * scale / 2;

      canvas.style = "transform:" +
        "translate(" + transformX + "px," + transformY + "px) " +
        "scale(" + scale + ")";
    };
    
    return {
      renderValue: function(x) {
        code = x.code;
        
        if (x.svg) {
          wrapper = document.createElementNS("http://www.w3.org/2000/svg", "svg");
          wrapper.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xlink", "http://www.w3.org/1999/xlink");
          el.appendChild(wrapper);
          if (x.className) el.classList.add(x.className);
          
          g = document.createElementNS("http://www.w3.org/2000/svg", "g");
          g.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xlink", "http://www.w3.org/1999/xlink");
          wrapper.appendChild(g);
          
          g.innerHTML = nomnoml.renderSvg(x.code);
          
          svg = g.childNodes[0];
          resizeSvg(width, height);
        }
        else {
          canvas = document.createElement("canvas");
          el.appendChild(canvas);
          
          resizeCanvas(width, height);
        }
      },

      resize: function(width, height) {
        if (canvas)
          resizeCanvas(width, height);
        else
          resizeSvg(width, height);
      }
    };
  }
});