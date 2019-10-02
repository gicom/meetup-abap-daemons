sap.ui.define([
    "jquery.sap.global",
    "sap/ui/core/UIComponent",
    "sap/ui/core/routing/HashChanger",
    "sap/ui/core/mvc/XMLView",
    "sap/ui/Device",
    "sap/m/Shell",
    "sap/m/App"
], function (jQuery, UIComponent, HashChanger, XMLView, Device, Shell, App) {
    "use strict";

    return UIComponent.extend("app.Component", {
        metadata: {
            manifest: "json"
        },

        createContent: function () {
            const oView = new XMLView({viewName: "app.view.Main"});

            return new Shell("shell", {
                showLogout: false,
                app: new App("app", {
                    pages: [ oView ],
                    initialPage: oView
                })
            });
        }
    });
});