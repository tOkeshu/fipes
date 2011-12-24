(function() {

    App.Models.Files = Backbone.Collection.extend({
        model: App.Models.File,

        url: function() {
            // TODO: throw a comprehensive error when the fipe was not
            // attached.
            return this.fipe.url() + '/files';
        },

        save: function() {
            // Save files, only when there is an attached fipe.
            if (this.fipe) {
                this.each(function(file) {
                    if (file.isNew()) file.save();
                });
            }
        }
    });

    App.Models.File = Backbone.Model.extend({
        toJSON: function() {
            return {
                name : this.get('name'),
                type : this.get('type'),
                size : this.get('size')
            };
        }
    });

})();
