/**
 * Main Fipes JS namespace
 */
window.App = {
    Models: {}
};

App.Router = Backbone.Router.extend({
    routes: {
        "/"      : "root",
        "/fipes" : "fipes"
    },

    root: function() {
        App.Routes.navigate('/fipes', true);
    },

    fipes: function() {
        var fipe = new App.Models.Fipe;
        fipe.save();
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

