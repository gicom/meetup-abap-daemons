sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel",
    "sap/m/MessageBox",
    "sap/m/Dialog",
    "sap/m/Label",
    "sap/m/Input",
    "sap/m/InputType",
    "sap/m/Button",
    "sap/m/ButtonType",
    "jquery.sap.global",
    "sap/ui/core/ws/SapPcpWebSocket"
], function (Controller, JSONModel, MessageBox, Dialog, Label, Input, InputType, Button, ButtonType, jQuery, WebSocket) {
    "use strict";

    const MAX_TWEETS = 35;

    return Controller.extend("app.controller.Main", {
        /**
         * Called when this controller is instantiated.
         * @public
         */
        onInit() {
            this.getView().setModel(new JSONModel([]), "feed");
            this.initializeWebSocket();
        },

        getFeedModel() {
            return this.getView().getModel("feed");
        },

        askForUserDetails(fnCallback) {
            const oInputUsername = new Input({
                placeholder: "SAP Username"
            });

            const oInputPassword = new Input({
                placeholder: "SAP Password",
                type: InputType.Password
            });

            const oDialog = new Dialog({
                title: "Authentication",
                content: [
                    new Label({
                        text: "Please enter your SAP username and password:"
                    }),
                    oInputUsername,
                    oInputPassword
                ],
                beginButton: new Button({
                    text: "OK",
                    type: ButtonType.Emphasized,
                    press: () => {
                        fnCallback(oInputUsername.getValue(), oInputPassword.getValue());
                        oDialog.close();
                    }
                })
            });

            oDialog.open();
        },

        initializeWebSocket() {
            const sHost = "gicomsap18.gicom.local";
            const iPort = 8000;

            this.askForUserDetails((sUsername, sPassword) => {
                const oWebSocket = new WebSocket(`ws://${sUsername}:${sPassword}@${sHost}:${iPort}/sap/bc/apc/sap/zos_tweet_apc`, sap.ui.core.ws.SapPcpWebSocket.SUPPORTED_PROTOCOLS.v10);

                oWebSocket.attachOpen(() => {
                    console.log("Connection opened");
                });

                oWebSocket.attachMessage(oEvent => {
                    console.log("Got message");

                    const oTweet = JSON.parse(oEvent.getParameter("data"));
                    this.handleTweet(oTweet);
                });
            });
        },

        handleTweet(oTweet) {
            this.addTweet(oTweet);
            this.scrollToTop();
        },

        addTweet(oTweet) {
            const aTweets = this.getFeedModel().getData();
            aTweets.splice(0, 0, oTweet);
            if (aTweets.length > MAX_TWEETS) {
                aTweets.splice(aTweets.length - 1, 1);
            }
            this.getFeedModel().setData(aTweets);
        },

        scrollToTop() {
            this.getView().byId("scrollContainer").scrollTo(0, 0);
        }
    });
});