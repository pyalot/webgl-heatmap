class Shader
    constructor: (@gl, {vertex, fragment}) ->
        @program    = @gl.createProgram()
        @vs         = @gl.createShader @gl.VERTEX_SHADER
        @fs         = @gl.createShader @gl.FRAGMENT_SHADER
        @gl.attachShader @program, @vs
        @gl.attachShader @program, @fs
        @compileShader @vs, vertex
        @compileShader @fs, fragment
        @link()

        @value_cache = {}
        @uniform_cache = {}
        @attribCache = {}
    
    attribLocation: (name) ->
        location = @attribCache[name]
        if location is undefined
            location = @attribCache[name] = @gl.getAttribLocation @program, name
        return location
    
    compileShader: (shader, source) ->
        @gl.shaderSource shader, source
        @gl.compileShader shader

        if not @gl.getShaderParameter shader, @gl.COMPILE_STATUS
            throw "Shader Compile Error: #{@gl.getShaderInfoLog(shader)}"
    
    link: ->
        @gl.linkProgram @program

        if not @gl.getProgramParameter @program, @gl.LINK_STATUS
            throw "Shader Link Error: #{@gl.getProgramInfoLog(@program)}"

    use: ->
        @gl.useProgram @program
        return @
    
    uniformLoc: (name) ->
        location = @uniform_cache[name]
        if location is undefined
            location = @uniform_cache[name] = @gl.getUniformLocation @program, name
        return location
    
    int: (name, value) ->
        cached = @value_cache[name]
        if cached != value
            @value_cache[name] = value
            loc = @uniformLoc name
            @gl.uniform1i loc, value if loc
        return @
    
    vec2: (name, a, b) ->
        loc = @uniformLoc name
        @gl.uniform2f loc, a, b if loc
        return @
            
    float: (name, value) ->
        cached = @value_cache[name]
        if cached != value
            @value_cache[name] = value
            loc = @uniformLoc name
            @gl.uniform1f loc, value if loc
        return @

class Framebuffer
    constructor: (@gl) ->
        @buffer = @gl.createFramebuffer()

    destroy: ->
        @gl.deleteFRamebuffer @buffer

    bind: ->
        @gl.bindFramebuffer @gl.FRAMEBUFFER, @buffer
        return @

    unbind: ->
        @gl.bindFramebuffer @gl.FRAMEBUFFER, null
        return @

    check: ->
        result = @gl.checkFramebufferStatus @gl.FRAMEBUFFER
        switch result
            when @gl.FRAMEBUFFER_UNSUPPORTED
                throw 'Framebuffer is unsupported'
            when @gl.FRAMEBUFFER_INCOMPLETE_ATTACHMENT
                throw 'Framebuffer incomplete attachment'
            when @gl.FRAMEBUFFER_INCOMPLETE_DIMENSIONS
                throw 'Framebuffer incomplete dimensions'
            when @gl.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
                throw 'Framebuffer incomplete missing attachment'
        return @

    color: (texture) ->
        @gl.framebufferTexture2D @gl.FRAMEBUFFER, @gl.COLOR_ATTACHMENT0, texture.target, texture.handle, 0
        @check()
        return @
    
    depth: (buffer) ->
        @gl.framebufferRenderbuffer @gl.FRAMEBUFFER, @gl.DEPTH_ATTACHMENT, @gl.RENDERBUFFER, buffer.id
        @check()
        return @

    destroy: ->
        @gl.deleteFramebuffer @buffer

class Texture
    constructor: (@gl, params={}) ->
        @channels = @gl[(params.channels ? 'rgb').toUpperCase()]
        @type = @gl[(params.type ? 'unsigned_byte').toUpperCase()]

        switch @channels
            when @gl.RGBA then @chancount = 4
            when @gl.RGB then @chancount = 3
            when @gl.LUMINANCE_ALPHA then @chancount = 2
            else @chancount = 1

        @target = @gl.TEXTURE_2D
        @handle = @gl.createTexture()

    destroy: ->
        @gl.deleteTexture @handle
    
    bind: (unit=0) ->
        if unit > 15
            throw 'Texture unit too large: ' + unit

        @gl.activeTexture @gl.TEXTURE0+unit
        @gl.bindTexture @target, @handle

        return @

    setSize: (@width, @height) ->
        @gl.texImage2D @target, 0, @channels, @width, @height, 0, @channels, @type, null
        return @
    
    linear: ->
        @gl.texParameteri @target, @gl.TEXTURE_MAG_FILTER, @gl.LINEAR
        @gl.texParameteri @target, @gl.TEXTURE_MIN_FILTER, @gl.LINEAR
        return @
    
    nearest: ->
        @gl.texParameteri @target, @gl.TEXTURE_MAG_FILTER, @gl.NEAREST
        @gl.texParameteri @target, @gl.TEXTURE_MIN_FILTER, @gl.NEAREST
        return @

    clampToEdge: ->
        @gl.texParameteri @target, @gl.TEXTURE_WRAP_S, @gl.CLAMP_TO_EDGE
        @gl.texParameteri @target, @gl.TEXTURE_WRAP_T, @gl.CLAMP_TO_EDGE
        return @
    
    repeat: ->
        @gl.texParameteri @target, @gl.TEXTURE_WRAP_S, @gl.REPEAT
        @gl.texParameteri @target, @gl.TEXTURE_WRAP_T, @gl.REPEAT
        return @

class Node
    constructor: (@gl, @width, @height) ->
        @texture = new Texture(@gl, type:'float').bind(0).setSize(@width, @height).nearest().clampToEdge()
        try
            @fbo = new Framebuffer(@gl).bind().color(@texture).unbind()
        catch error
            throw 'Floating point render target not supported'

    use: -> @fbo.bind()
    bind: (unit) -> @texture.bind(unit)
    end: -> @fbo.unbind()

    resize: (@width, @height) ->
        @texture.bind(0).setSize(@width, @height)

vertexShaderBlit = '''
    attribute vec4 position;
    varying vec2 texcoord;
    void main(){
        texcoord = position.xy*0.5+0.5;
        gl_Position = position;
    }
'''

fragmentShaderBlit = '''
    precision highp int;
    precision highp float;
    precision highp vec2;
    precision highp vec3;
    precision highp vec4;
    uniform sampler2D source;
    varying vec2 texcoord;
'''

class Heights
    constructor: (@heatmap, @gl, @width, @height) ->
        @shader = new Shader @gl,
            vertex: '''
                attribute vec4 position, intensity;
                varying vec2 off, dim;
                varying float vIntensity;
                uniform vec2 viewport;

                void main(){
                    dim = abs(position.zw);
                    off = position.zw;
                    vec2 pos = position.xy + position.zw;
                    vIntensity = intensity.x;
                    gl_Position = vec4((pos/viewport)*2.0-1.0, 0.0, 1.0);
                }
            '''
            fragment: '''
                precision highp int;
                precision highp float;
                precision highp vec2;
                precision highp vec3;
                precision highp vec4;
                varying vec2 off, dim;
                varying float vIntensity;
                void main(){
                    float falloff = (1.0 - smoothstep(0.0, 1.0, length(off/dim)));
                    float intensity = falloff*vIntensity;
                    gl_FragColor = vec4(intensity);
                }
            '''

        @clampShader = new Shader @gl,
            vertex: vertexShaderBlit
            fragment: fragmentShaderBlit + '''
                uniform float low, high;
                void main(){
                    gl_FragColor = vec4(clamp(texture2D(source, texcoord).rgb, low, high), 1.0);
                }
            '''
        
        @multiplyShader = new Shader @gl,
            vertex: vertexShaderBlit
            fragment: fragmentShaderBlit + '''
                uniform float value;
                void main(){
                    gl_FragColor = vec4(texture2D(source, texcoord).rgb*value, 1.0);
                }
            '''
        
        @blurShader = new Shader @gl,
            vertex: vertexShaderBlit
            fragment: fragmentShaderBlit + '''
                uniform vec2 viewport;
                void main(){
                    vec4 result = vec4(0.0);
                    for(int x=-1; x<=1; x++){
                        for(int y=-1; y<=1; y++){
                            vec2 off = vec2(x,y)/viewport;
                            //float factor = 1.0 - smoothstep(0.0, 1.5, length(off));
                            float factor = 1.0;
                            result += vec4(texture2D(source, texcoord+off).rgb*factor, factor);
                        }
                    }
                    gl_FragColor = vec4(result.rgb/result.w, 1.0);
                }
            '''

        @nodeBack = new Node @gl, @width, @height
        @nodeFront = new Node @gl, @width, @height
        
        @vertexBuffer = @gl.createBuffer()
        @vertexSize = 8
        @maxPointCount = 1024*10
        @vertexBufferData = new Float32Array @maxPointCount*@vertexSize*6
        @vertexBufferViews = []
        for i in [0...@maxPointCount]
            @vertexBufferViews.push new Float32Array(@vertexBufferData.buffer, 0, i*@vertexSize*6)

        @bufferIndex = 0
        @pointCount = 0

    resize: (@width, @height) ->
        @nodeBack.resize @width, @height
        @nodeFront.resize @width, @height

    update: ->
        if @pointCount > 0
            @gl.enable @gl.BLEND

            @nodeFront.use()

            @gl.bindBuffer @gl.ARRAY_BUFFER, @vertexBuffer
            @gl.bufferData @gl.ARRAY_BUFFER, @vertexBufferViews[@pointCount], @gl.STREAM_DRAW

            positionLoc = @shader.attribLocation('position')
            intensityLoc = @shader.attribLocation('intensity')
            
            @gl.enableVertexAttribArray 1
            @gl.vertexAttribPointer(positionLoc, 4, @gl.FLOAT, false, 8*4, 0*4)
            @gl.vertexAttribPointer(intensityLoc, 4, @gl.FLOAT, false, 8*4, 4*4)
            @shader.use().vec2('viewport', @width, @height)
            @gl.drawArrays @gl.TRIANGLES, 0, @pointCount*6
            @gl.disableVertexAttribArray 1

            @pointCount = 0
            @bufferIndex = 0

            @nodeFront.end()
            @gl.disable @gl.BLEND

    clear: ->
        @nodeFront.use()
        @gl.clearColor(0,0,0,1)
        @gl.clear(@gl.COLOR_BUFFER_BIT)
        @nodeFront.end()

    clamp: (min, max) ->
        @gl.bindBuffer @gl.ARRAY_BUFFER, @heatmap.quad
        @gl.vertexAttribPointer(0, 4, @gl.FLOAT, false, 0, 0)
        @nodeFront.bind(0)
        @nodeBack.use()
        @clampShader.use().int('source', 0).float('low', min).float('high', max)
        @gl.drawArrays @gl.TRIANGLES, 0, 6
        @nodeBack.end()
        @swap()
    
    multiply: (value) ->
        @gl.bindBuffer @gl.ARRAY_BUFFER, @heatmap.quad
        @gl.vertexAttribPointer(0, 4, @gl.FLOAT, false, 0, 0)
        @nodeFront.bind(0)
        @nodeBack.use()
        @multiplyShader.use().int('source', 0).float('value', value)
        @gl.drawArrays @gl.TRIANGLES, 0, 6
        @nodeBack.end()
        @swap()

    blur: ->
        @gl.bindBuffer @gl.ARRAY_BUFFER, @heatmap.quad
        @gl.vertexAttribPointer(0, 4, @gl.FLOAT, false, 0, 0)
        @nodeFront.bind(0)
        @nodeBack.use()
        @blurShader.use().int('source', 0).vec2('viewport', @width, @height)
        @gl.drawArrays @gl.TRIANGLES, 0, 6
        @nodeBack.end()
        @swap()

    swap: ->
        tmp = @nodeFront
        @nodeFront = @nodeBack
        @nodeBack = tmp

    addVertex: (x, y, xs, ys, intensity) ->
        @vertexBufferData[@bufferIndex++] = x
        @vertexBufferData[@bufferIndex++] = y
        @vertexBufferData[@bufferIndex++] = xs
        @vertexBufferData[@bufferIndex++] = ys
        @vertexBufferData[@bufferIndex++] = intensity
        @vertexBufferData[@bufferIndex++] = intensity
        @vertexBufferData[@bufferIndex++] = intensity
        @vertexBufferData[@bufferIndex++] = intensity

    addPoint: (x, y, size=50, intensity=0.2) ->
        if @pointCount >= @maxPointCount - 1
            @update()

        #if @pointCount < @maxPointCount
        y = @height - y
        s = size/2
        @addVertex x, y, -s, -s, intensity
        @addVertex x, y, +s, -s, intensity
        @addVertex x, y, -s, +s, intensity
        
        @addVertex x, y, -s, +s, intensity
        @addVertex x, y, +s, -s, intensity
        @addVertex x, y, +s, +s, intensity

        @pointCount += 1

class WebGLHeatmap
    constructor: ({@canvas, @width, @height}={}) ->
        @canvas = document.createElement('canvas') unless @canvas
        try
            @gl = @canvas.getContext('experimental-webgl', depth:false, antialias:false)
            if @gl == null
                @gl = @canvas.getContext('webgl', depth:false, antialias:false)
                if @gl == null
                    throw 'WebGL not supported'
        catch error
            throw 'WebGL not supported'

        if not @gl.getExtension('OES_texture_float')
            throw 'No floating point texture support'

        @gl.enableVertexAttribArray 0
        @gl.blendFunc @gl.ONE, @gl.ONE

        @shader = new Shader @gl,
            vertex: vertexShaderBlit
            fragment: fragmentShaderBlit + '''
                float linstep(float low, float high, float value){
                    return clamp((value-low)/(high-low), 0.0, 1.0);
                }

                float fade(float low, float high, float value){
                    float mid = (low+high)*0.5;
                    float range = (high-low)*0.5;
                    float x = 1.0 - clamp(abs(mid-value)/range, 0.0, 1.0);
                    return smoothstep(0.0, 1.0, x);
                }
                void main(){
                    float intensity = smoothstep(0.0, 1.0, texture2D(source, texcoord).r);

                    vec3 blue = vec3(0.0, 0.0, 1.0);
                    vec3 cyan = vec3(0.0, 1.0, 1.0);
                    vec3 green = vec3(0.0, 1.0, 0.0);
                    vec3 yellow = vec3(1.0, 1.0, 0.0);
                    vec3 red = vec3(1.0, 0.0, 0.0);

                    vec3 color = (
                        fade(-0.25, 0.25, intensity)*blue +
                        fade(0.0, 0.5, intensity)*cyan +
                        fade(0.25, 0.75, intensity)*green +
                        fade(0.5, 1.0, intensity)*yellow +
                        smoothstep(0.75, 1.0, intensity)*red
                    );

                    gl_FragColor = vec4(color*intensity, intensity);
                    //gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
                }
            '''

        @width ?= @canvas.offsetWidth or 2
        @height ?= @canvas.offsetHeight or 2
        @canvas.width = @width
        @canvas.height = @height
        @gl.viewport(0, 0, @width, @height)
        
        @quad = @gl.createBuffer()
        @gl.bindBuffer @gl.ARRAY_BUFFER, @quad
        quad = new Float32Array([
            -1, -1, 0, 1,
            1, -1, 0, 1,
            -1, 1, 0, 1,
            
            -1, 1, 0, 1,
            1, -1, 0, 1,
            1, 1, 0, 1,
        ])
        @gl.bufferData @gl.ARRAY_BUFFER, quad, @gl.STATIC_DRAW
        @gl.bindBuffer @gl.ARRAY_BUFFER, null
        
        @heights = new Heights @, @gl, @width, @height
    
    adjustSize: ->
        canvasWidth = @canvas.offsetWidth or 2
        canvasHeight = @canvas.offsetHeight or 2

        if @width isnt canvasWidth or @height isnt canvasHeight
            @gl.viewport 0, 0, canvasWidth, canvasHeight
            @canvas.width = canvasWidth
            @canvas.height = canvasHeight
            @width = canvasWidth
            @height = canvasHeight
            @heights.resize @width, @height

    display: ->
        @gl.bindBuffer @gl.ARRAY_BUFFER, @quad
        @gl.vertexAttribPointer(0, 4, @gl.FLOAT, false, 0, 0)
        @heights.nodeFront.bind(0)
        @shader.use().int('source', 0)
        @gl.drawArrays @gl.TRIANGLES, 0, 6

    update: ->
        @heights.update()

    clear: -> @heights.clear()
    clamp: (min=0, max=1) -> @heights.clamp(min, max)
    multiply: (value=0.95) -> @heights.multiply(value)
    blur: -> @heights.blur()

    addPoint: (x, y, size, intensity) -> @heights.addPoint x, y, size, intensity

window.createWebGLHeatmap = (params) -> new WebGLHeatmap(params)
