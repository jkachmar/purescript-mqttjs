"use strict";

// MQTT

var mqtt = require('mqtt');

exports._connect = function(url, opts) {
  return mqtt.connect(url, opts);
};

exports.onConnect = function(client, handler) {
  client.on('connect', function() {
    handler();
  });
};

exports.onMessage = function(client, handler) {
  client.on('message', function(topic, message) {
    handler(topic)(message)();
  });
};

exports.onClose = function(client, handler) {
  client.on('close', function() {
    handler();
  });
};

exports._subscribe = function(client, topic) {
  client.subscribe(topic);
};

exports._publish = function(client, topic, message) {
  client.publish(topic, message);
};

exports._end = function(client) {
  client.end();
};
