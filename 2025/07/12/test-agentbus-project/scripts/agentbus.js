#!/usr/bin/env node

// Simple agentbus coordination system
const fs = require('fs');
const path = require('path');

const AGENTBUS_DIR = path.join(process.cwd(), '.agentbus');
const MESSAGES_FILE = path.join(AGENTBUS_DIR, 'messages.json');
const FLAGS_FILE = path.join(AGENTBUS_DIR, 'flags.json');

// Ensure agentbus directory exists
if (!fs.existsSync(AGENTBUS_DIR)) {
    fs.mkdirSync(AGENTBUS_DIR, { recursive: true });
}

// Initialize files if they don't exist
if (!fs.existsSync(MESSAGES_FILE)) {
    fs.writeFileSync(MESSAGES_FILE, JSON.stringify([], null, 2));
}
if (!fs.existsSync(FLAGS_FILE)) {
    fs.writeFileSync(FLAGS_FILE, JSON.stringify({}, null, 2));
}

function addMessage(type, agent, topic, message) {
    const messages = JSON.parse(fs.readFileSync(MESSAGES_FILE, 'utf8'));
    const timestamp = new Date().toISOString();
    messages.push({ timestamp, type, agent, topic, message });
    fs.writeFileSync(MESSAGES_FILE, JSON.stringify(messages, null, 2));
    console.log(`[${type.toUpperCase()}] ${agent} -> ${topic}: ${message}`);
}

function setFlag(flag, value = true) {
    const flags = JSON.parse(fs.readFileSync(FLAGS_FILE, 'utf8'));
    flags[flag] = value;
    fs.writeFileSync(FLAGS_FILE, JSON.stringify(flags, null, 2));
    console.log(`Flag set: ${flag} = ${value}`);
}

const command = process.argv[2];
const args = process.argv.slice(3);

switch (command) {
    case 'announce':
        addMessage('announce', process.env.AGENT_ID || 'unknown', 'general', args.join(' '));
        break;
    case 'speak':
        if (args.length < 2) {
            console.error('Usage: speak <topic> <message>');
            process.exit(1);
        }
        const topic = args[0];
        const message = args.slice(1).join(' ');
        addMessage('speak', process.env.AGENT_ID || 'unknown', topic, message);
        break;
    case 'jot':
        addMessage('jot', process.env.AGENT_ID || 'unknown', 'notes', args.join(' '));
        break;
    case 'satisfy':
        if (args.length === 0) {
            console.error('Usage: satisfy <flag>');
            process.exit(1);
        }
        setFlag(args[0], true);
        break;
    case 'messages':
        const messages = JSON.parse(fs.readFileSync(MESSAGES_FILE, 'utf8'));
        console.log(JSON.stringify(messages, null, 2));
        break;
    case 'flags':
        const flags = JSON.parse(fs.readFileSync(FLAGS_FILE, 'utf8'));
        console.log(JSON.stringify(flags, null, 2));
        break;
    default:
        console.error('Unknown command:', command);
        console.error('Available commands: announce, speak, jot, satisfy, messages, flags');
        process.exit(1);
}
