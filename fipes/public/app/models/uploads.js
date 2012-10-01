(function() {

    App.Models.Upload = Backbone.Model.extend({

        initialize: function() {
            this.bind('change:seek', this.complete, this);
        },

        complete: function() {
            if (this.progress() == 100)
                this.trigger('complete', this);
        },

        progress: function() {
            return this.get('seek') * 100 / this.get('size');
        }

    });

    App.Models.Uploads = Backbone.Collection.extend({
        model: App.Models.Upload
    });

})();
