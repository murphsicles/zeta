const express = require('express');
const path = require('path');
const WebSocket = require('ws');
const http = require('http');
const axios = require('axios');

const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

// OpenClaw API configuration
const OPENCLAW_API = 'http://127.0.0.1:18789';

// Serve static files
app.use(express.static(path.join(__dirname)));

// API endpoints for Mission Control
app.get('/api/status', async (req, res) => {
    try {
        // Try to get real status from OpenClaw
        const openclawStatus = await axios.get(`${OPENCLAW_API}/api/status`);
        
        // Get sessions list to count real agents
        const sessions = await axios.get(`${OPENCLAW_API}/api/sessions`, {
            params: { activeMinutes: 60 }
        });
        
        const realAgents = sessions.data.filter(s => 
            s.sessionKey.includes('agent:') && 
            !s.sessionKey.includes('gateway')
        ).length;
        
        res.json({
            system: 'operational',
            agents: realAgents,
            tasks: 12, // TODO: Get from OpenClaw task queue
            uptime: process.uptime(),
            timestamp: new Date().toISOString(),
            openclaw: openclawStatus.data,
            dataSource: 'real'
        });
    } catch (error) {
        // Fallback to simulated data if OpenClaw unavailable
        console.error('OpenClaw API error:', error.message);
        res.json({
            system: 'operational',
            agents: 5,
            tasks: 12,
            uptime: process.uptime(),
            timestamp: new Date().toISOString(),
            dataSource: 'simulated',
            warning: 'OpenClaw API unavailable, using simulated data'
        });
    }
});

app.get('/api/agents', async (req, res) => {
    try {
        // Get real sessions from OpenClaw
        const sessions = await axios.get(`${OPENCLAW_API}/api/sessions`, {
            params: { activeMinutes: 60, limit: 50 }
        });
        
        // Map OpenClaw sessions to Mission Control agents
        const agents = sessions.data
            .filter(s => s.sessionKey.includes('agent:'))
            .map((session, index) => {
                // Determine agent name from session key
                let name = 'Unknown Agent';
                let role = 'Agent';
                let avatar = '🤖';
                let status = session.lastActivity ? 'online' : 'offline';
                
                // Special handling for known agents
                if (session.sessionKey.includes('lex') || session.sessionKey.includes('LEX')) {
                    name = 'LEX';
                    role = 'Zeta\'s Code Guru';
                    avatar = '👨‍💻';
                } else if (session.sessionKey.includes('main')) {
                    name = 'Zak (Dark Factory)';
                    role = 'Firstborn / Gatekeeper';
                    avatar = '👑';
                } else if (session.sessionKey.includes('subagent')) {
                    name = `Worker ${index + 1}`;
                    role = 'Specialized Worker';
                    avatar = '👷';
                }
                
                return {
                    id: index + 1,
                    name,
                    status,
                    role,
                    avatar,
                    sessionKey: session.sessionKey,
                    lastActivity: session.lastActivity
                };
            });
        
        // Ensure LEX is always in the list (even if not currently active)
        const hasLEX = agents.some(a => a.name === 'LEX');
        if (!hasLEX) {
            agents.unshift({
                id: 0,
                name: 'LEX',
                status: 'offline',
                role: 'Zeta\'s Code Guru',
                avatar: '👨‍💻',
                sessionKey: 'agent:lex',
                lastActivity: null
            });
        }
        
        res.json(agents);
    } catch (error) {
        // Fallback to simulated data with LEX included
        console.error('OpenClaw API error:', error.message);
        res.json([
            { id: 1, name: 'Zak (Dark Factory)', status: 'online', role: 'Firstborn / Gatekeeper', avatar: '👑' },
            { id: 2, name: 'LEX', status: 'busy', role: 'Zeta\'s Code Guru', avatar: '👨‍💻' },
            { id: 3, name: 'Bootstrap Worker', status: 'busy', role: '24/7 Compiler', avatar: '⚙️' },
            { id: 4, name: 'CI Monitor', status: 'online', role: 'Quality Assurance', avatar: '📊' },
            { id: 5, name: 'Memory Keeper', status: 'online', role: 'Context Manager', avatar: '🧠' },
            { id: 6, name: 'Git Guardian', status: 'offline', role: 'Repository Manager', avatar: '🔒' }
        ]);
    }
});

app.get('/api/tasks', (req, res) => {
    res.json([
        { id: 1, title: 'Fix v0.3.7 Const Parsing', status: 'active', priority: 'high', assignedTo: 'Dark Factory' },
        { id: 2, title: 'Build Mission Control v2', status: 'active', priority: 'high', assignedTo: 'Dark Factory' },
        { id: 3, title: 'Setup Zeta Foundation Council', status: 'pending', priority: 'medium', assignedTo: 'Memory Keeper' },
        { id: 4, title: 'Create Specialized Agents', status: 'pending', priority: 'medium', assignedTo: 'Dark Factory' },
        { id: 5, title: 'Connect to OpenClaw API', status: 'completed', priority: 'low', assignedTo: 'CI Monitor' }
    ]);
});

app.get('/api/branches', (req, res) => {
    res.json([
        { 
            name: 'main', 
            description: 'Pure Zeta Language (v0.5.0+)', 
            status: 'active',
            lastCommit: 'Just now',
            ciStatus: 'passing',
            purpose: 'Production Zeta language definition',
            rules: 'ONLY .z files, NO Rust code'
        },
        { 
            name: 'v0.3.8', 
            description: 'Active Compiler Development', 
            status: 'active',
            lastCommit: '2 hours ago',
            ciStatus: 'pending',
            purpose: 'v0.3.8 compiler implementation',
            rules: 'Rust .rs files + zeta_src/ Zeta source'
        },
        { 
            name: 'dev', 
            description: 'Experimental Work', 
            status: 'active',
            lastCommit: '1 day ago',
            ciStatus: 'passing',
            purpose: 'Experiments, WIP, testing',
            rules: 'Anything goes (keep organized)'
        },
        { 
            name: 'stable', 
            description: 'Reference Only', 
            status: 'archive',
            lastCommit: '1 week ago',
            ciStatus: 'n/a',
            purpose: 'Archive of stable releases',
            rules: 'Read-only, no development'
        }
    ]);
});

app.get('/api/tasks', (req, res) => {
    res.json([
        { id: 1, title: 'Train all agents on new branch structure', status: 'active', priority: 'high', assignedTo: 'Zak', branch: 'all' },
        { id: 2, title: 'Migrate night shift work to v0.3.8 branch', status: 'pending', priority: 'high', assignedTo: 'LEX', branch: 'v0.3.8' },
        { id: 3, title: 'Update CI workflows for new branches', status: 'pending', priority: 'medium', assignedTo: 'CI Monitor', branch: 'all' },
        { id: 4, title: 'Verify branch separation is maintained', status: 'active', priority: 'high', assignedTo: 'Git Guardian', branch: 'all' },
        { id: 5, title: 'Continue v0.3.8 compiler improvements', status: 'pending', priority: 'medium', assignedTo: 'LEX', branch: 'v0.3.8' }
    ]);
});

app.get('/api/activities', (req, res) => {
    const activities = [
        { time: 'Just now', action: 'Branch structure reorganized - 4 active branches', agent: 'Zak' },
        { time: '5 min ago', action: 'Deleted old branches (bootstrap-work, development, etc.)', agent: 'Zak' },
        { time: '10 min ago', action: 'Created v0.3.8 branch from release/v0.3.7-final-bootstrap', agent: 'Zak' },
        { time: '12 min ago', action: 'Created dev branch from development', agent: 'Zak' },
        { time: '15 min ago', action: 'Training LEX on new branch structure', agent: 'Zak' },
        { time: '30 min ago', action: 'Fixed branch contamination in main', agent: 'Zak' },
        { time: '2 hours ago', action: 'Night shift: v0.3.8 improvements completed', agent: 'Zeta Implementer' },
        { time: '3 hours ago', action: 'Night shift: Bootstrap analysis completed', agent: 'Bootstrap Analyst' }
    ];
    res.json(activities);
});

// WebSocket for real-time updates
wss.on('connection', (ws) => {
    console.log('New WebSocket connection');
    
    // Send initial data
    ws.send(JSON.stringify({
        type: 'welcome',
        message: 'Connected to OpenClaw Mission Control',
        timestamp: new Date().toISOString()
    }));

    // Send periodic updates
    const interval = setInterval(() => {
        if (ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify({
                type: 'update',
                data: {
                    systemStatus: 'operational',
                    activeAgents: Math.floor(Math.random() * 3) + 3,
                    pendingTasks: Math.floor(Math.random() * 5) + 8,
                    memoryUsage: `${Math.floor(Math.random() * 30) + 30}%`,
                    timestamp: new Date().toISOString()
                }
            }));
        }
    }, 10000); // Every 10 seconds

    ws.on('close', () => {
        console.log('WebSocket disconnected');
        clearInterval(interval);
    });
});

// Serve main page
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'index.html'));
});

const PORT = process.env.PORT || 3000;
server.listen(PORT, () => {
    console.log(`🚀 Mission Control running at http://localhost:${PORT}`);
    console.log(`📡 WebSocket server ready on port ${PORT}`);
    console.log(`📊 Dashboard: http://localhost:${PORT}/`);
    console.log(`🔌 API: http://localhost:${PORT}/api/status`);
});

// Simulate some activity
setInterval(() => {
    // Broadcast activity to all connected clients
    const activity = {
        type: 'activity',
        data: {
            time: new Date().toLocaleTimeString(),
            action: ['Heartbeat check', 'Agent status update', 'Task progress', 'Memory sync'][Math.floor(Math.random() * 4)],
            agent: ['Dark Factory', 'Bootstrap Worker', 'CI Monitor', 'System'][Math.floor(Math.random() * 4)]
        }
    };

    wss.clients.forEach(client => {
        if (client.readyState === WebSocket.OPEN) {
            client.send(JSON.stringify(activity));
        }
    });
}, 15000); // Every 15 seconds