var Framebuffer, Heights, Node, Shader, Texture, WebGLHeatmap, fragmentShaderBlit, nukeVendorPrefix, textureFloatShims, vertexShaderBlit,
  __indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

nukeVendorPrefix = function() {
  var getExtension, getSupportedExtensions, vendorRe, vendors;
  if (window.WebGLRenderingContext != null) {
    vendors = ['WEBKIT', 'MOZ', 'MS', 'O'];
    vendorRe = /^WEBKIT_(.*)|MOZ_(.*)|MS_(.*)|O_(.*)/;
    getExtension = WebGLRenderingContext.prototype.getExtension;
    WebGLRenderingContext.prototype.getExtension = function(name) {
      var extobj, match, vendor, _i, _len;
      match = name.match(vendorRe);
      if (match !== null) {
        name = match[1];
      }
      extobj = getExtension.call(this, name);
      if (extobj === null) {
        for (_i = 0, _len = vendors.length; _i < _len; _i++) {
          vendor = vendors[_i];
          extobj = getExtension.call(this, vendor + '_' + name);
          if (extobj !== null) {
            return extobj;
          }
        }
        return null;
      } else {
        return extobj;
      }
    };
    getSupportedExtensions = WebGLRenderingContext.prototype.getSupportedExtensions;
    return WebGLRenderingContext.prototype.getSupportedExtensions = function() {
      var extension, match, result, supported, _i, _len;
      supported = getSupportedExtensions.call(this);
      result = [];
      for (_i = 0, _len = supported.length; _i < _len; _i++) {
        extension = supported[_i];
        match = extension.match(vendorRe);
        if (match !== null) {
          extension = match[1];
        }
        if (__indexOf.call(result, extension) < 0) {
          result.push(extension);
        }
      }
      return result;
    };
  }
};

textureFloatShims = function() {
  var checkColorBuffer, checkFloatLinear, checkSupport, checkTexture, createSourceCanvas, getExtension, getSupportedExtensions, name, shimExtensions, shimLookup, unshimExtensions, unshimLookup, _i, _len;
  createSourceCanvas = function() {
    var canvas, ctx, imageData;
    canvas = document.createElement('canvas');
    canvas.width = 2;
    canvas.height = 2;
    ctx = canvas.getContext('2d');
    imageData = ctx.getImageData(0, 0, 2, 2);
    imageData.data.set(new Uint8ClampedArray([0, 0, 0, 0, 255, 255, 255, 255, 0, 0, 0, 0, 255, 255, 255, 255]));
    ctx.putImageData(imageData, 0, 0);
    return canvas;
  };
  createSourceCanvas();
  checkFloatLinear = function(gl, sourceType) {
    var buffer, cleanup, fragmentShader, framebuffer, positionLoc, program, readBuffer, result, source, sourceCanvas, sourceLoc, target, vertexShader, vertices;
    program = gl.createProgram();
    vertexShader = gl.createShader(gl.VERTEX_SHADER);
    gl.attachShader(program, vertexShader);
    gl.shaderSource(vertexShader, 'attribute vec2 position;\nvoid main(){\n    gl_Position = vec4(position, 0.0, 1.0);\n}');
    gl.compileShader(vertexShader);
    if (!gl.getShaderParameter(vertexShader, gl.COMPILE_STATUS)) {
      throw gl.getShaderInfoLog(vertexShader);
    }
    fragmentShader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.attachShader(program, fragmentShader);
    gl.shaderSource(fragmentShader, 'uniform sampler2D source;\nvoid main(){\n    gl_FragColor = texture2D(source, vec2(1.0, 1.0));\n}');
    gl.compileShader(fragmentShader);
    if (!gl.getShaderParameter(fragmentShader, gl.COMPILE_STATUS)) {
      throw gl.getShaderInfoLog(fragmentShader);
    }
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      throw gl.getProgramInfoLog(program);
    }
    gl.useProgram(program);
    cleanup = function() {
      gl.deleteShader(fragmentShader);
      gl.deleteShader(vertexShader);
      gl.deleteProgram(program);
      gl.deleteBuffer(buffer);
      gl.deleteTexture(source);
      gl.deleteTexture(target);
      gl.deleteFramebuffer(framebuffer);
      gl.bindBuffer(gl.ARRAY_BUFFER, null);
      gl.useProgram(null);
      gl.bindTexture(gl.TEXTURE_2D, null);
      return gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    };
    target = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, target);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 2, 2, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    framebuffer = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, target, 0);
    sourceCanvas = createSourceCanvas();
    source = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, source);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, sourceType, sourceCanvas);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    vertices = new Float32Array([1, 1, -1, 1, -1, -1, 1, 1, -1, -1, 1, -1]);
    buffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);
    positionLoc = gl.getAttribLocation(program, 'position');
    sourceLoc = gl.getUniformLocation(program, 'source');
    gl.enableVertexAttribArray(positionLoc);
    gl.vertexAttribPointer(positionLoc, 2, gl.FLOAT, false, 0, 0);
    gl.uniform1i(sourceLoc, 0);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    readBuffer = new Uint8Array(4 * 4);
    gl.readPixels(0, 0, 2, 2, gl.RGBA, gl.UNSIGNED_BYTE, readBuffer);
    result = Math.abs(readBuffer[0] - 127) < 10;
    cleanup();
    return result;
  };
  checkTexture = function(gl, targetType) {
    var target;
    target = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, target);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 2, 2, 0, gl.RGBA, targetType, null);
    if (gl.getError() === 0) {
      gl.deleteTexture(target);
      return true;
    } else {
      gl.deleteTexture(target);
      return false;
    }
  };
  checkColorBuffer = function(gl, targetType) {
    var check, framebuffer, target;
    target = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, target);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 2, 2, 0, gl.RGBA, targetType, null);
    framebuffer = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, target, 0);
    check = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
    gl.deleteTexture(target);
    gl.deleteFramebuffer(framebuffer);
    gl.bindTexture(gl.TEXTURE_2D, null);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    if (check === gl.FRAMEBUFFER_COMPLETE) {
      return true;
    } else {
      return false;
    }
  };
  shimExtensions = [];
  shimLookup = {};
  unshimExtensions = [];
  checkSupport = function() {
    var canvas, extobj, gl, halfFloatExt, halfFloatTexturing, singleFloatExt, singleFloatTexturing;
    canvas = document.createElement('canvas');
    gl = null;
    try {
      gl = canvas.getContext('experimental-webgl');
      if (gl === null) {
        gl = canvas.getContext('webgl');
      }
    } catch (_error) {}
    if (gl != null) {
      singleFloatExt = gl.getExtension('OES_texture_float');
      if (singleFloatExt === null) {
        if (checkTexture(gl, gl.FLOAT)) {
          singleFloatTexturing = true;
          shimExtensions.push('OES_texture_float');
          shimLookup.OES_texture_float = {
            shim: true
          };
        } else {
          singleFloatTexturing = false;
          unshimExtensions.push('OES_texture_float');
        }
      } else {
        if (checkTexture(gl, gl.FLOAT)) {
          singleFloatTexturing = true;
          shimExtensions.push('OES_texture_float');
        } else {
          singleFloatTexturing = false;
          unshimExtensions.push('OES_texture_float');
        }
      }
      if (singleFloatTexturing) {
        extobj = gl.getExtension('WEBGL_color_buffer_float');
        if (extobj === null) {
          if (checkColorBuffer(gl, gl.FLOAT)) {
            shimExtensions.push('WEBGL_color_buffer_float');
            shimLookup.WEBGL_color_buffer_float = {
              shim: true,
              RGBA32F_EXT: 0x8814,
              RGB32F_EXT: 0x8815,
              FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT: 0x8211,
              UNSIGNED_NORMALIZED_EXT: 0x8C17
            };
          } else {
            unshimExtensions.push('WEBGL_color_buffer_float');
          }
        } else {
          if (checkColorBuffer(gl, gl.FLOAT)) {
            shimExtensions.push('WEBGL_color_buffer_float');
          } else {
            unshimExtensions.push('WEBGL_color_buffer_float');
          }
        }
        extobj = gl.getExtension('OES_texture_float_linear');
        if (extobj === null) {
          if (checkFloatLinear(gl, gl.FLOAT)) {
            shimExtensions.push('OES_texture_float_linear');
            shimLookup.OES_texture_float_linear = {
              shim: true
            };
          } else {
            unshimExtensions.push('OES_texture_float_linear');
          }
        } else {
          if (checkFloatLinear(gl, gl.FLOAT)) {
            shimExtensions.push('OES_texture_float_linear');
          } else {
            unshimExtensions.push('OES_texture_float_linear');
          }
        }
      }
      halfFloatExt = gl.getExtension('OES_texture_half_float');
      if (halfFloatExt === null) {
        if (checkTexture(gl, 0x8D61)) {
          halfFloatTexturing = true;
          shimExtensions.push('OES_texture_half_float');
          halfFloatExt = shimLookup.OES_texture_half_float = {
            HALF_FLOAT_OES: 0x8D61,
            shim: true
          };
        } else {
          halfFloatTexturing = false;
          unshimExtensions.push('OES_texture_half_float');
        }
      } else {
        if (checkTexture(gl, halfFloatExt.HALF_FLOAT_OES)) {
          halfFloatTexturing = true;
          shimExtensions.push('OES_texture_half_float');
        } else {
          halfFloatTexturing = false;
          unshimExtensions.push('OES_texture_half_float');
        }
      }
      if (halfFloatTexturing) {
        extobj = gl.getExtension('EXT_color_buffer_half_float');
        if (extobj === null) {
          if (checkColorBuffer(gl, halfFloatExt.HALF_FLOAT_OES)) {
            shimExtensions.push('EXT_color_buffer_half_float');
            shimLookup.EXT_color_buffer_half_float = {
              shim: true,
              RGBA16F_EXT: 0x881A,
              RGB16F_EXT: 0x881B,
              FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT: 0x8211,
              UNSIGNED_NORMALIZED_EXT: 0x8C17
            };
          } else {
            unshimExtensions.push('EXT_color_buffer_half_float');
          }
        } else {
          if (checkColorBuffer(gl, halfFloatExt.HALF_FLOAT_OES)) {
            shimExtensions.push('EXT_color_buffer_half_float');
          } else {
            unshimExtensions.push('EXT_color_buffer_half_float');
          }
        }
        extobj = gl.getExtension('OES_texture_half_float_linear');
        if (extobj === null) {
          if (checkFloatLinear(gl, halfFloatExt.HALF_FLOAT_OES)) {
            shimExtensions.push('OES_texture_half_float_linear');
            return shimLookup.OES_texture_half_float_linear = {
              shim: true
            };
          } else {
            return unshimExtensions.push('OES_texture_half_float_linear');
          }
        } else {
          if (checkFloatLinear(gl, halfFloatExt.HALF_FLOAT_OES)) {
            return shimExtensions.push('OES_texture_half_float_linear');
          } else {
            return unshimExtensions.push('OES_texture_half_float_linear');
          }
        }
      }
    }
  };
  if (window.WebGLRenderingContext != null) {
    checkSupport();
    unshimLookup = {};
    for (_i = 0, _len = unshimExtensions.length; _i < _len; _i++) {
      name = unshimExtensions[_i];
      unshimLookup[name] = true;
    }
    getExtension = WebGLRenderingContext.prototype.getExtension;
    WebGLRenderingContext.prototype.getExtension = function(name) {
      var extobj;
      extobj = shimLookup[name];
      if (extobj === void 0) {
        if (unshimLookup[name]) {
          return null;
        } else {
          return getExtension.call(this, name);
        }
      } else {
        return extobj;
      }
    };
    getSupportedExtensions = WebGLRenderingContext.prototype.getSupportedExtensions;
    WebGLRenderingContext.prototype.getSupportedExtensions = function() {
      var extension, result, supported, _j, _k, _len1, _len2;
      supported = getSupportedExtensions.call(this);
      result = [];
      for (_j = 0, _len1 = supported.length; _j < _len1; _j++) {
        extension = supported[_j];
        if (unshimLookup[extension] === void 0) {
          result.push(extension);
        }
      }
      for (_k = 0, _len2 = shimExtensions.length; _k < _len2; _k++) {
        extension = shimExtensions[_k];
        if (__indexOf.call(result, extension) < 0) {
          result.push(extension);
        }
      }
      return result;
    };
    return WebGLRenderingContext.prototype.getFloatExtension = function(spec) {
      var candidate, candidates, half, halfFramebuffer, halfLinear, halfTexture, i, importance, preference, result, single, singleFramebuffer, singleLinear, singleTexture, use, _j, _k, _l, _len1, _len2, _len3, _len4, _m, _ref, _ref1, _ref2;
      if (spec.prefer == null) {
        spec.prefer = ['half'];
      }
      if (spec.require == null) {
        spec.require = [];
      }
      if (spec.throws == null) {
        spec.throws = true;
      }
      singleTexture = this.getExtension('OES_texture_float');
      halfTexture = this.getExtension('OES_texture_half_float');
      singleFramebuffer = this.getExtension('WEBGL_color_buffer_float');
      halfFramebuffer = this.getExtension('EXT_color_buffer_half_float');
      singleLinear = this.getExtension('OES_texture_float_linear');
      halfLinear = this.getExtension('OES_texture_half_float_linear');
      single = {
        texture: singleTexture !== null,
        filterable: singleLinear !== null,
        renderable: singleFramebuffer !== null,
        score: 0,
        precision: 'single',
        half: false,
        single: true,
        type: this.FLOAT
      };
      half = {
        texture: halfTexture !== null,
        filterable: halfLinear !== null,
        renderable: halfFramebuffer !== null,
        score: 0,
        precision: 'half',
        half: true,
        single: false,
        type: (_ref = halfTexture != null ? halfTexture.HALF_FLOAT_OES : void 0) != null ? _ref : null
      };
      candidates = [];
      if (single.texture) {
        candidates.push(single);
      }
      if (half.texture) {
        candidates.push(half);
      }
      result = [];
      for (_j = 0, _len1 = candidates.length; _j < _len1; _j++) {
        candidate = candidates[_j];
        use = true;
        _ref1 = spec.require;
        for (_k = 0, _len2 = _ref1.length; _k < _len2; _k++) {
          name = _ref1[_k];
          if (candidate[name] === false) {
            use = false;
          }
        }
        if (use) {
          result.push(candidate);
        }
      }
      for (_l = 0, _len3 = result.length; _l < _len3; _l++) {
        candidate = result[_l];
        _ref2 = spec.prefer;
        for (i = _m = 0, _len4 = _ref2.length; _m < _len4; i = ++_m) {
          preference = _ref2[i];
          importance = Math.pow(2, spec.prefer.length - i - 1);
          if (candidate[preference]) {
            candidate.score += importance;
          }
        }
      }
      result.sort(function(a, b) {
        if (a.score === b.score) {
          return 0;
        } else if (a.score < b.score) {
          return 1;
        } else if (a.score > b.score) {
          return -1;
        }
      });
      if (result.length === 0) {
        if (spec.throws) {
          throw 'No floating point texture support that is ' + spec.require.join(', ');
        } else {
          return null;
        }
      } else {
        result = result[0];
        return {
          filterable: result.filterable,
          renderable: result.renderable,
          type: result.type,
          precision: result.precision
        };
      }
    };
  }
};

nukeVendorPrefix();

textureFloatShims();

Shader = (function() {
  function Shader(gl, _arg) {
    var fragment, vertex;
    this.gl = gl;
    vertex = _arg.vertex, fragment = _arg.fragment;
    this.program = this.gl.createProgram();
    this.vs = this.gl.createShader(this.gl.VERTEX_SHADER);
    this.fs = this.gl.createShader(this.gl.FRAGMENT_SHADER);
    this.gl.attachShader(this.program, this.vs);
    this.gl.attachShader(this.program, this.fs);
    this.compileShader(this.vs, vertex);
    this.compileShader(this.fs, fragment);
    this.link();
    this.value_cache = {};
    this.uniform_cache = {};
    this.attribCache = {};
  }

  Shader.prototype.attribLocation = function(name) {
    var location;
    location = this.attribCache[name];
    if (location === void 0) {
      location = this.attribCache[name] = this.gl.getAttribLocation(this.program, name);
    }
    return location;
  };

  Shader.prototype.compileShader = function(shader, source) {
    this.gl.shaderSource(shader, source);
    this.gl.compileShader(shader);
    if (!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)) {
      throw "Shader Compile Error: " + (this.gl.getShaderInfoLog(shader));
    }
  };

  Shader.prototype.link = function() {
    this.gl.linkProgram(this.program);
    if (!this.gl.getProgramParameter(this.program, this.gl.LINK_STATUS)) {
      throw "Shader Link Error: " + (this.gl.getProgramInfoLog(this.program));
    }
  };

  Shader.prototype.use = function() {
    this.gl.useProgram(this.program);
    return this;
  };

  Shader.prototype.uniformLoc = function(name) {
    var location;
    location = this.uniform_cache[name];
    if (location === void 0) {
      location = this.uniform_cache[name] = this.gl.getUniformLocation(this.program, name);
    }
    return location;
  };

  Shader.prototype.int = function(name, value) {
    var cached, loc;
    cached = this.value_cache[name];
    if (cached !== value) {
      this.value_cache[name] = value;
      loc = this.uniformLoc(name);
      if (loc) {
        this.gl.uniform1i(loc, value);
      }
    }
    return this;
  };

  Shader.prototype.vec2 = function(name, a, b) {
    var loc;
    loc = this.uniformLoc(name);
    if (loc) {
      this.gl.uniform2f(loc, a, b);
    }
    return this;
  };

  Shader.prototype.float = function(name, value) {
    var cached, loc;
    cached = this.value_cache[name];
    if (cached !== value) {
      this.value_cache[name] = value;
      loc = this.uniformLoc(name);
      if (loc) {
        this.gl.uniform1f(loc, value);
      }
    }
    return this;
  };

  return Shader;

})();

Framebuffer = (function() {
  function Framebuffer(gl) {
    this.gl = gl;
    this.buffer = this.gl.createFramebuffer();
  }

  Framebuffer.prototype.destroy = function() {
    return this.gl.deleteFRamebuffer(this.buffer);
  };

  Framebuffer.prototype.bind = function() {
    this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.buffer);
    return this;
  };

  Framebuffer.prototype.unbind = function() {
    this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
    return this;
  };

  Framebuffer.prototype.check = function() {
    var result;
    result = this.gl.checkFramebufferStatus(this.gl.FRAMEBUFFER);
    switch (result) {
      case this.gl.FRAMEBUFFER_UNSUPPORTED:
        throw 'Framebuffer is unsupported';
        break;
      case this.gl.FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
        throw 'Framebuffer incomplete attachment';
        break;
      case this.gl.FRAMEBUFFER_INCOMPLETE_DIMENSIONS:
        throw 'Framebuffer incomplete dimensions';
        break;
      case this.gl.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
        throw 'Framebuffer incomplete missing attachment';
    }
    return this;
  };

  Framebuffer.prototype.color = function(texture) {
    this.gl.framebufferTexture2D(this.gl.FRAMEBUFFER, this.gl.COLOR_ATTACHMENT0, texture.target, texture.handle, 0);
    this.check();
    return this;
  };

  Framebuffer.prototype.depth = function(buffer) {
    this.gl.framebufferRenderbuffer(this.gl.FRAMEBUFFER, this.gl.DEPTH_ATTACHMENT, this.gl.RENDERBUFFER, buffer.id);
    this.check();
    return this;
  };

  Framebuffer.prototype.destroy = function() {
    return this.gl.deleteFramebuffer(this.buffer);
  };

  return Framebuffer;

})();

Texture = (function() {
  function Texture(gl, params) {
    var _ref, _ref1;
    this.gl = gl;
    if (params == null) {
      params = {};
    }
    this.channels = this.gl[((_ref = params.channels) != null ? _ref : 'rgba').toUpperCase()];
    if (typeof params.type === 'number') {
      this.type = params.type;
    } else {
      this.type = this.gl[((_ref1 = params.type) != null ? _ref1 : 'unsigned_byte').toUpperCase()];
    }
    switch (this.channels) {
      case this.gl.RGBA:
        this.chancount = 4;
        break;
      case this.gl.RGB:
        this.chancount = 3;
        break;
      case this.gl.LUMINANCE_ALPHA:
        this.chancount = 2;
        break;
      default:
        this.chancount = 1;
    }
    this.target = this.gl.TEXTURE_2D;
    this.handle = this.gl.createTexture();
  }

  Texture.prototype.destroy = function() {
    return this.gl.deleteTexture(this.handle);
  };

  Texture.prototype.bind = function(unit) {
    if (unit == null) {
      unit = 0;
    }
    if (unit > 15) {
      throw 'Texture unit too large: ' + unit;
    }
    this.gl.activeTexture(this.gl.TEXTURE0 + unit);
    this.gl.bindTexture(this.target, this.handle);
    return this;
  };

  Texture.prototype.setSize = function(width, height) {
    this.width = width;
    this.height = height;
    this.gl.texImage2D(this.target, 0, this.channels, this.width, this.height, 0, this.channels, this.type, null);
    return this;
  };

  Texture.prototype.upload = function(data) {
    this.width = data.width;
    this.height = data.height;
    this.gl.texImage2D(this.target, 0, this.channels, this.channels, this.type, data);
    return this;
  };

  Texture.prototype.linear = function() {
    this.gl.texParameteri(this.target, this.gl.TEXTURE_MAG_FILTER, this.gl.LINEAR);
    this.gl.texParameteri(this.target, this.gl.TEXTURE_MIN_FILTER, this.gl.LINEAR);
    return this;
  };

  Texture.prototype.nearest = function() {
    this.gl.texParameteri(this.target, this.gl.TEXTURE_MAG_FILTER, this.gl.NEAREST);
    this.gl.texParameteri(this.target, this.gl.TEXTURE_MIN_FILTER, this.gl.NEAREST);
    return this;
  };

  Texture.prototype.clampToEdge = function() {
    this.gl.texParameteri(this.target, this.gl.TEXTURE_WRAP_S, this.gl.CLAMP_TO_EDGE);
    this.gl.texParameteri(this.target, this.gl.TEXTURE_WRAP_T, this.gl.CLAMP_TO_EDGE);
    return this;
  };

  Texture.prototype.repeat = function() {
    this.gl.texParameteri(this.target, this.gl.TEXTURE_WRAP_S, this.gl.REPEAT);
    this.gl.texParameteri(this.target, this.gl.TEXTURE_WRAP_T, this.gl.REPEAT);
    return this;
  };

  return Texture;

})();

Node = (function() {
  function Node(gl, width, height) {
    var floatExt;
    this.gl = gl;
    this.width = width;
    this.height = height;
    floatExt = this.gl.getFloatExtension({
      require: ['renderable']
    });
    this.texture = new Texture(this.gl, {
      type: floatExt.type
    }).bind(0).setSize(this.width, this.height).nearest().clampToEdge();
    this.fbo = new Framebuffer(this.gl).bind().color(this.texture).unbind();
  }

  Node.prototype.use = function() {
    return this.fbo.bind();
  };

  Node.prototype.bind = function(unit) {
    return this.texture.bind(unit);
  };

  Node.prototype.end = function() {
    return this.fbo.unbind();
  };

  Node.prototype.resize = function(width, height) {
    this.width = width;
    this.height = height;
    return this.texture.bind(0).setSize(this.width, this.height);
  };

  return Node;

})();

vertexShaderBlit = 'attribute vec4 position;\nvarying vec2 texcoord;\nvoid main(){\n    texcoord = position.xy*0.5+0.5;\n    gl_Position = position;\n}';

fragmentShaderBlit = '#ifdef GL_FRAGMENT_PRECISION_HIGH\n    precision highp int;\n    precision highp float;\n#else\n    precision mediump int;\n    precision mediump float;\n#endif\nuniform sampler2D source;\nvarying vec2 texcoord;';

Heights = (function() {
  function Heights(heatmap, gl, width, height) {
    var i, _i, _ref;
    this.heatmap = heatmap;
    this.gl = gl;
    this.width = width;
    this.height = height;
    this.shader = new Shader(this.gl, {
      vertex: 'attribute vec4 position, intensity;\nvarying vec2 off, dim;\nvarying float vIntensity;\nuniform vec2 viewport;\n\nvoid main(){\n    dim = abs(position.zw);\n    off = position.zw;\n    vec2 pos = position.xy + position.zw;\n    vIntensity = intensity.x;\n    gl_Position = vec4((pos/viewport)*2.0-1.0, 0.0, 1.0);\n}',
      fragment: '#ifdef GL_FRAGMENT_PRECISION_HIGH\n    precision highp int;\n    precision highp float;\n#else\n    precision mediump int;\n    precision mediump float;\n#endif\nvarying vec2 off, dim;\nvarying float vIntensity;\nvoid main(){\n    float falloff = (1.0 - smoothstep(0.0, 1.0, length(off/dim)));\n    float intensity = falloff*vIntensity;\n    gl_FragColor = vec4(intensity);\n}'
    });
    this.clampShader = new Shader(this.gl, {
      vertex: vertexShaderBlit,
      fragment: fragmentShaderBlit + 'uniform float low, high;\nvoid main(){\n    gl_FragColor = vec4(clamp(texture2D(source, texcoord).rgb, low, high), 1.0);\n}'
    });
    this.multiplyShader = new Shader(this.gl, {
      vertex: vertexShaderBlit,
      fragment: fragmentShaderBlit + 'uniform float value;\nvoid main(){\n    gl_FragColor = vec4(texture2D(source, texcoord).rgb*value, 1.0);\n}'
    });
    this.blurShader = new Shader(this.gl, {
      vertex: vertexShaderBlit,
      fragment: fragmentShaderBlit + 'uniform vec2 viewport;\nvoid main(){\n    vec4 result = vec4(0.0);\n    for(int x=-1; x<=1; x++){\n        for(int y=-1; y<=1; y++){\n            vec2 off = vec2(x,y)/viewport;\n            //float factor = 1.0 - smoothstep(0.0, 1.5, length(off));\n            float factor = 1.0;\n            result += vec4(texture2D(source, texcoord+off).rgb*factor, factor);\n        }\n    }\n    gl_FragColor = vec4(result.rgb/result.w, 1.0);\n}'
    });
    this.nodeBack = new Node(this.gl, this.width, this.height);
    this.nodeFront = new Node(this.gl, this.width, this.height);
    this.vertexBuffer = this.gl.createBuffer();
    this.vertexSize = 8;
    this.maxPointCount = 1024 * 10;
    this.vertexBufferData = new Float32Array(this.maxPointCount * this.vertexSize * 6);
    this.vertexBufferViews = [];
    for (i = _i = 0, _ref = this.maxPointCount; 0 <= _ref ? _i < _ref : _i > _ref; i = 0 <= _ref ? ++_i : --_i) {
      this.vertexBufferViews.push(new Float32Array(this.vertexBufferData.buffer, 0, i * this.vertexSize * 6));
    }
    this.bufferIndex = 0;
    this.pointCount = 0;
  }

  Heights.prototype.resize = function(width, height) {
    this.width = width;
    this.height = height;
    this.nodeBack.resize(this.width, this.height);
    return this.nodeFront.resize(this.width, this.height);
  };

  Heights.prototype.update = function() {
    var intensityLoc, positionLoc;
    if (this.pointCount > 0) {
      this.gl.enable(this.gl.BLEND);
      this.nodeFront.use();
      this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.vertexBuffer);
      this.gl.bufferData(this.gl.ARRAY_BUFFER, this.vertexBufferViews[this.pointCount], this.gl.STREAM_DRAW);
      positionLoc = this.shader.attribLocation('position');
      intensityLoc = this.shader.attribLocation('intensity');
      this.gl.enableVertexAttribArray(1);
      this.gl.vertexAttribPointer(positionLoc, 4, this.gl.FLOAT, false, 8 * 4, 0 * 4);
      this.gl.vertexAttribPointer(intensityLoc, 4, this.gl.FLOAT, false, 8 * 4, 4 * 4);
      this.shader.use().vec2('viewport', this.width, this.height);
      this.gl.drawArrays(this.gl.TRIANGLES, 0, this.pointCount * 6);
      this.gl.disableVertexAttribArray(1);
      this.pointCount = 0;
      this.bufferIndex = 0;
      this.nodeFront.end();
      return this.gl.disable(this.gl.BLEND);
    }
  };

  Heights.prototype.clear = function() {
    this.nodeFront.use();
    this.gl.clearColor(0, 0, 0, 1);
    this.gl.clear(this.gl.COLOR_BUFFER_BIT);
    return this.nodeFront.end();
  };

  Heights.prototype.clamp = function(min, max) {
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.heatmap.quad);
    this.gl.vertexAttribPointer(0, 4, this.gl.FLOAT, false, 0, 0);
    this.nodeFront.bind(0);
    this.nodeBack.use();
    this.clampShader.use().int('source', 0).float('low', min).float('high', max);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
    this.nodeBack.end();
    return this.swap();
  };

  Heights.prototype.multiply = function(value) {
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.heatmap.quad);
    this.gl.vertexAttribPointer(0, 4, this.gl.FLOAT, false, 0, 0);
    this.nodeFront.bind(0);
    this.nodeBack.use();
    this.multiplyShader.use().int('source', 0).float('value', value);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
    this.nodeBack.end();
    return this.swap();
  };

  Heights.prototype.blur = function() {
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.heatmap.quad);
    this.gl.vertexAttribPointer(0, 4, this.gl.FLOAT, false, 0, 0);
    this.nodeFront.bind(0);
    this.nodeBack.use();
    this.blurShader.use().int('source', 0).vec2('viewport', this.width, this.height);
    this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
    this.nodeBack.end();
    return this.swap();
  };

  Heights.prototype.swap = function() {
    var tmp;
    tmp = this.nodeFront;
    this.nodeFront = this.nodeBack;
    return this.nodeBack = tmp;
  };

  Heights.prototype.addVertex = function(x, y, xs, ys, intensity) {
    this.vertexBufferData[this.bufferIndex++] = x;
    this.vertexBufferData[this.bufferIndex++] = y;
    this.vertexBufferData[this.bufferIndex++] = xs;
    this.vertexBufferData[this.bufferIndex++] = ys;
    this.vertexBufferData[this.bufferIndex++] = intensity;
    this.vertexBufferData[this.bufferIndex++] = intensity;
    this.vertexBufferData[this.bufferIndex++] = intensity;
    return this.vertexBufferData[this.bufferIndex++] = intensity;
  };

  Heights.prototype.addPoint = function(x, y, size, intensity) {
    var s;
    if (size == null) {
      size = 50;
    }
    if (intensity == null) {
      intensity = 0.2;
    }
    if (this.pointCount >= this.maxPointCount - 1) {
      this.update();
    }
    y = this.height - y;
    s = size / 2;
    this.addVertex(x, y, -s, -s, intensity);
    this.addVertex(x, y, +s, -s, intensity);
    this.addVertex(x, y, -s, +s, intensity);
    this.addVertex(x, y, -s, +s, intensity);
    this.addVertex(x, y, +s, -s, intensity);
    this.addVertex(x, y, +s, +s, intensity);
    return this.pointCount += 1;
  };

  return Heights;

})();

WebGLHeatmap = (function() {
  function WebGLHeatmap(_arg) {
    var alphaEnd, alphaRange, alphaStart, error, getColorFun, gradientTexture, image, intensityToAlpha, output, quad, textureGradient, _ref, _ref1;
    _ref = _arg != null ? _arg : {}, this.canvas = _ref.canvas, this.width = _ref.width, this.height = _ref.height, intensityToAlpha = _ref.intensityToAlpha, gradientTexture = _ref.gradientTexture, alphaRange = _ref.alphaRange;
    if (!this.canvas) {
      this.canvas = document.createElement('canvas');
    }
    try {
      this.gl = this.canvas.getContext('experimental-webgl', {
        depth: false,
        antialias: false
      });
      if (this.gl === null) {
        this.gl = this.canvas.getContext('webgl', {
          depth: false,
          antialias: false
        });
        if (this.gl === null) {
          throw 'WebGL not supported';
        }
      }
    } catch (_error) {
      error = _error;
      throw 'WebGL not supported';
    }
    if (window.WebGLDebugUtils != null) {
      console.log('debugging mode');
      this.gl = WebGLDebugUtils.makeDebugContext(this.gl, function(err, funcName, args) {
        throw WebGLDebugUtils.glEnumToString(err) + " was caused by call to: " + funcName;
      });
    }
    this.gl.enableVertexAttribArray(0);
    this.gl.blendFunc(this.gl.ONE, this.gl.ONE);
    if (gradientTexture) {
      textureGradient = this.gradientTexture = new Texture(this.gl, {
        channels: 'rgba'
      }).bind(0).setSize(2, 2).nearest().clampToEdge();
      if (typeof gradientTexture === 'string') {
        image = new Image();
        image.onload = function() {
          return textureGradient.bind().upload(image);
        };
        image.src = gradientTexture;
      } else {
        if (gradientTexture.width > 0 && gradientTexture.height > 0) {
          textureGradient.upload(gradientTexture);
        } else {
          gradientTexture.onload = function() {
            return textureGradient.upload(gradientTexture);
          };
        }
      }
      getColorFun = 'uniform sampler2D gradientTexture;\nvec3 getColor(float intensity){\n    return texture2D(gradientTexture, vec2(intensity, 0.0)).rgb;\n}';
    } else {
      textureGradient = null;
      getColorFun = 'vec3 getColor(float intensity){\n    vec3 blue = vec3(0.0, 0.0, 1.0);\n    vec3 cyan = vec3(0.0, 1.0, 1.0);\n    vec3 green = vec3(0.0, 1.0, 0.0);\n    vec3 yellow = vec3(1.0, 1.0, 0.0);\n    vec3 red = vec3(1.0, 0.0, 0.0);\n\n    vec3 color = (\n        fade(-0.25, 0.25, intensity)*blue +\n        fade(0.0, 0.5, intensity)*cyan +\n        fade(0.25, 0.75, intensity)*green +\n        fade(0.5, 1.0, intensity)*yellow +\n        smoothstep(0.75, 1.0, intensity)*red\n    );\n    return color;\n}';
    }
    if (intensityToAlpha == null) {
      intensityToAlpha = true;
    }
    if (intensityToAlpha) {
      _ref1 = alphaRange != null ? alphaRange : [0, 1], alphaStart = _ref1[0], alphaEnd = _ref1[1];
      output = "vec4 alphaFun(vec3 color, float intensity){\n    float alpha = smoothstep(" + (alphaStart.toFixed(8)) + ", " + (alphaEnd.toFixed(8)) + ", intensity);\n    return vec4(color*alpha, alpha);\n}";
    } else {
      output = 'vec4 alphaFun(vec3 color, float intensity){\n    return vec4(color, 1.0);\n}';
    }
    this.shader = new Shader(this.gl, {
      vertex: vertexShaderBlit,
      fragment: fragmentShaderBlit + ("float linstep(float low, float high, float value){\n    return clamp((value-low)/(high-low), 0.0, 1.0);\n}\n\nfloat fade(float low, float high, float value){\n    float mid = (low+high)*0.5;\n    float range = (high-low)*0.5;\n    float x = 1.0 - clamp(abs(mid-value)/range, 0.0, 1.0);\n    return smoothstep(0.0, 1.0, x);\n}\n\n" + getColorFun + "\n" + output + "\n\nvoid main(){\n    float intensity = smoothstep(0.0, 1.0, texture2D(source, texcoord).r);\n    vec3 color = getColor(intensity);\n    gl_FragColor = alphaFun(color, intensity);\n}")
    });
    if (this.width == null) {
      this.width = this.canvas.offsetWidth || 2;
    }
    if (this.height == null) {
      this.height = this.canvas.offsetHeight || 2;
    }
    this.canvas.width = this.width;
    this.canvas.height = this.height;
    this.gl.viewport(0, 0, this.width, this.height);
    this.quad = this.gl.createBuffer();
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.quad);
    quad = new Float32Array([-1, -1, 0, 1, 1, -1, 0, 1, -1, 1, 0, 1, -1, 1, 0, 1, 1, -1, 0, 1, 1, 1, 0, 1]);
    this.gl.bufferData(this.gl.ARRAY_BUFFER, quad, this.gl.STATIC_DRAW);
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, null);
    this.heights = new Heights(this, this.gl, this.width, this.height);
  }

  WebGLHeatmap.prototype.adjustSize = function() {
    var canvasHeight, canvasWidth;
    canvasWidth = this.canvas.offsetWidth || 2;
    canvasHeight = this.canvas.offsetHeight || 2;
    if (this.width !== canvasWidth || this.height !== canvasHeight) {
      this.gl.viewport(0, 0, canvasWidth, canvasHeight);
      this.canvas.width = canvasWidth;
      this.canvas.height = canvasHeight;
      this.width = canvasWidth;
      this.height = canvasHeight;
      return this.heights.resize(this.width, this.height);
    }
  };

  WebGLHeatmap.prototype.display = function() {
    this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.quad);
    this.gl.vertexAttribPointer(0, 4, this.gl.FLOAT, false, 0, 0);
    this.heights.nodeFront.bind(0);
    if (this.gradientTexture) {
      this.gradientTexture.bind(1);
    }
    this.shader.use().int('source', 0).int('gradientTexture', 1);
    return this.gl.drawArrays(this.gl.TRIANGLES, 0, 6);
  };

  WebGLHeatmap.prototype.update = function() {
    return this.heights.update();
  };

  WebGLHeatmap.prototype.clear = function() {
    return this.heights.clear();
  };

  WebGLHeatmap.prototype.clamp = function(min, max) {
    if (min == null) {
      min = 0;
    }
    if (max == null) {
      max = 1;
    }
    return this.heights.clamp(min, max);
  };

  WebGLHeatmap.prototype.multiply = function(value) {
    if (value == null) {
      value = 0.95;
    }
    return this.heights.multiply(value);
  };

  WebGLHeatmap.prototype.blur = function() {
    return this.heights.blur();
  };

  WebGLHeatmap.prototype.addPoint = function(x, y, size, intensity) {
    return this.heights.addPoint(x, y, size, intensity);
  };

  WebGLHeatmap.prototype.addPoints = function(items) {
    var item, _i, _len, _results;
    _results = [];
    for (_i = 0, _len = items.length; _i < _len; _i++) {
      item = items[_i];
      _results.push(this.addPoint(item.x, item.y, item.size, item.intensity));
    }
    return _results;
  };

  return WebGLHeatmap;

})();

window.createWebGLHeatmap = function(params) {
  return new WebGLHeatmap(params);
};