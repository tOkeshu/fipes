/**
 * Main Fipes JS namespace
 */
window.App = {
    // Namespaces
    Models    : {},
    Views     : {},

    // Instances
    Files     : undefined,
    FipeView  : undefined,
    FilesView : undefined
};

App.Router = Backbone.Router.extend({
    routes: {
        "/"            : "root",
        "/fipes"       : "fipes",
        "/fipes/:fipe" : "fipe"
    },

    root: function() {
        App.Routes.navigate('/fipes', true);
    },

    fipes: function() {
        App.Files     = new App.Models.Files;

        App.FipeView  = new App.Views.Fipe({el: $('#main')});
        App.FilesView = new App.Views.Files({
            el         : $('#main ul.files'),
            collection : App.Files
        });
    },

    fipe: function(fid) {
        if (App.Files === undefined) {
            this.fipes();
        }

        var fipe       = new App.Models.Fipe({id: fid});
        App.Files.fipe = App.Fipe = fipe;

        // There is no files yet, sync with the server.
        if (App.Files.length === 0) {
            App.Files.fetch();
        // There is files already, so we save them as we created the
        // fipe.
        } else {
            App.Files.save();
        }
    }

});

/**
 * Helpers. Highly dangerous stuff.
 */
App.Helpers = {};

/**
 * Misc. behaviours, such as global animations, routing...
 */
$(document).ready(function() {
    Backbone.sync = Backbone.TNetStrings.sync;
    App.Routes = new App.Router;

    if (!Backbone.history.start()) {
        App.Routes.navigate('/', true);
    }
});

