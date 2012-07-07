(function() {

    App.Views.Uploads = Backbone.View.extend({

        initialize: function() {
            this.collection.bind('add', _.bind(this.render, this));
            this.collection.bind('remove', _.bind(this.render, this));
            this.collection.bind('complete', _.bind(this.remove, this));
        },

        remove: function(upload) {
            that = this;
            setTimeout(function() {
                that.collection.remove(upload);
            }, 1000)
        },

        render: function() {
            $(this.el).empty()
            this.collection.each(_.bind(function(upload) {
                var view = new App.Views.Upload({model: upload});
                $(this.el).append(view.el);
                view.render();
            }, this));

            return this;
        }

    });

    App.Views.Upload = Backbone.View.extend({

        tagName: "li",

        _template: function() {
            return _.template($('#upload-template').text());
        },

        initialize: function() {
            this.model.bind('change:seek',  _.bind(this.updateProgress, this));
        },

        render: function() {
            var progress = this.model.get('seek') * 100 / this.model.get('size');
            var context = {progress: progress};
            $(this.el).empty().append((this._template())(context));
            return this;
        },

        updateProgress: function() {
            $(this.el).find('.bar').css('width', this.model.progress() + '%');
        },

    });
})();
