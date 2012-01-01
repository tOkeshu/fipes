(function() {

    App.Views.Files = Backbone.View.extend({

        initialize: function() {
            this.collection.bind('all', _.bind(this.render, this));
        },

        // XXX: Maybe split this function into several ones.
        // (ex: add, remove).
        render: function() {
            $(this.el).empty()
            this.collection.each(_.bind(function(file) {
                var view = new App.Views.File({model: file});
                $(this.el).append(view.el);
                view.render();
            }, this));

            return this;
        }

    });

    App.Views.File = Backbone.View.extend({

        tagName: "li",

        _template: function() {
            return _.template($('#file-template').text());
        },

        initialize: function() {
            this.model.bind('destroy', _.bind(this.remove, this));
            this.model.bind('change',  _.bind(this.render, this));
        },

        render: function() {
            var context = _.extend(this.model.toJSON(), {
                isNew : this.model.isNew(),
                url   : (App.Fipe) ? this.model.url() : undefined
            });
            $(this.el).empty().append((this._template())(context));
            return this;
        },

        remove: function() {
            $(this.el).remove();
        }

    });
})();
