(function() {

    App.Views.Fipe = Backbone.View.extend({
        events: {
            // Classic file selection
            'click    .upload a'          : 'browseFiles',
            'change   input[type="file"]' : 'enterTheFipe',
            // Drag'n drop
            'dragover .widget'            : 'dragOver',
            'drop     .widget'            : 'drop'
        },

        browseFiles: function(event) {
            var fileInput = $(this.el).find('input[type="file"]')[0];
            fileInput.click();
        },

        enterTheFipe: function(event) {
            this.addFiles(event.target.files);

            // Only redirect when we are on the home page.
            if (App.Fipe === undefined) {
                this.redirect();
            }
        },

        dragOver: function(event) {
            return false;
        },

        drop: function(event) {
            this.addFiles(event.dataTransfer.files);
        },

        addFiles: function(files) {
            files = _.map(files, function(file) {
                return new App.Models.File({
                    obj  : file,
                    name : file.name,
                    type : file.type,
                    size : file.size
                });
            });

            App.Files.add(files).save();
        },

        redirect: function() {
            var fipe = new App.Models.Fipe;
            fipe.save({}, {
                success: function() {
                    App.Routes.navigate(fipe.url(), true);
                }
            });
        }
    });
})();
