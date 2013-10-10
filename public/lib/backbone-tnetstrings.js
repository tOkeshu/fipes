(function(){

    // Map from CRUD to HTTP for our `Backbone.TNetStrings.sync`
    // implementation.
    var methodMap = {
        'create': 'POST',
        'update': 'PUT',
        'delete': 'DELETE',
        'read'  : 'GET'
    };

    // Turn on `emulateTNetStrings` to support legacy servers that
    // can't deal with direct `application/tnetstrings` requests
    // ... will encode the body as `application/x-www-form-urlencoded`
    // instead and will send the model in a form param named `model`.
    Backbone.emulateTNetStrings = false;


    // Backbone.TNetStrings
    // --------------------

    Backbone.TNetStrings = {

        // Backbone-TNetStrings.sync
        // -------------

        // This method is very similar to the original Backbone.sync
        // but use the TNetStrings serialisation format instead of
        // JSON. If you are not familiar with TNetStrings,
        // http://tnetstrings.org/ is a good starting point.
        //
        // It supports the Backbone.emulateHTTP and
        // Backbone.emulateTNetStrings options.
        sync: function(method, model, options, error) {
            options || (options = {});

            var type = methodMap[method];

            // Default TNetStrings-request options.
            var params = {type : type, dataType : 'tnetstrings'};

            // Ensure that we have a URL.
            if (!options.url) {
                params.url = getUrl(model) || urlError();
            }

            // Ensure that we have the appropriate request data.
            if (!options.data && model && (method == 'create' || method == 'update')) {
                params.contentType = 'application/tnetstrings';
                params.data = tnetstrings.dump(model.toJSON());
            }

            // For older servers, emulate JSON by encoding the request into an HTML-form.
            if (Backbone.emulateTNetStrings) {
                params.contentType = 'application/x-www-form-urlencoded';
                params.data = params.data ? {model : params.data} : {};
            }

            // For older servers, emulate HTTP by mimicking the HTTP method with `_method`
            // And an `X-HTTP-Method-Override` header.
            if (Backbone.emulateHTTP) {
                if (type === 'PUT' || type === 'DELETE') {
                    if (Backbone.emulateTNetStrings) params.data._method = type;
                    params.type = 'POST';
                    params.beforeSend = function(xhr) {
                        xhr.setRequestHeader('X-HTTP-Method-Override', type);
                    };
                }
            }

            var success = options.success
            options.success = function(resp, status, xhr) {
                var resp = tnetstrings.parse(resp).value;
                if (success) success(resp, status, xhr);
            }

            // Make the request, allowing the user to override any Ajax options.
            return $.ajax(_.extend(params, options));
        }
    };

    // Helpers
    // -------

    // Helper function to get a URL from a Model or Collection as a property
    // or as a function.
    var getUrl = function(object) {
        if (!(object && object.url)) return null;
        return _.isFunction(object.url) ? object.url() : object.url;
    };

    // Throw an error when a URL is needed, and none is supplied.
    var urlError = function() {
        throw new Error('A "url" property or function must be specified');
    };

}).call(this);

