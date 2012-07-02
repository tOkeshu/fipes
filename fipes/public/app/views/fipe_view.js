(function() {

    App.Views.Fipe = Backbone.View.extend({
        events: {
            // Classic file selection
            'click     .widget'          : 'browseFiles',
            'change    input[type="file"]' : 'selectFiles',
            // Drag'n drop
            'dragleave .widget'            : 'dragLeave',
            'dragover  .widget'            : 'dragOver',
            'drop      .widget'            : 'dropFiles'
        },

        browseFiles: function(event) {
            var fileInput = $(this.el).find('input[type="file"]')[0];
            fileInput.click();
        },

        selectFiles: function(event) {
            this.enterTheFipe(event.target.files);
        },

        dragLeave: function(event) {
            $(this.el).find('.dropzone').removeClass('active');
        },

        dragOver: function(event) {
            $(this.el).find('.dropzone').addClass('active');
            return false;
        },

        dropFiles: function(event) {
            $(this.el).find('.dropzone').removeClass('active');
            this.enterTheFipe(event.dataTransfer.files);
            return false;
        },

        enterTheFipe: function(files) {
            this.addFiles(files);

            // Only redirect when we are on the home page.
            if (App.Fipe === undefined) {
                this.redirect();
            }
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
