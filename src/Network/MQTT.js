"use strict";

// MQTT

var mqtt = require('async-mqtt');

exports._connect = function(url, opts) {
  return mqtt.connect(url, opts);
};

exports._onConnectEff = function(client, handler) {
  client.on('connect', function() {
    handler();
  });
};

exports._onConnectAff = function(client, handler) {
  return new Promise(function (resolve, reject) {
    try {
      client.on('connect', function() {
        resolve(handler());
      });
    } catch(error) {
      reject(error);
    }
  });
};

exports._onMessageEff = function(client, handler) {
  client.on('message', function(topic, message) {
    handler(topic)(message)();
  });
};

exports._onMessageAff = function(client, handler) {
  return new Promise(function (resolve, reject) {
    try {
      client.on('message', function(topic, message) {
        resolve(handler(topic)(message)());
      });
    } catch(error) {
      reject(error);
    }
  });
};

exports._subscribe = function(client, topic) {
  return client.subscribe(topic);
};

exports._publish = function(client, topic, message) {
  return client.publish(topic, message);
};

exports._end = function(client) {
  client.end();
};
