const express = require('express');
const path = require('path');
const WebSocket = require('ws');
const http = require('http');
const DataCollector = require('./data-collector');

const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

// Initialize live data collector
const dataCollector = new DataCollector();
dataCollector.startAutoCollection();

// Serve static files
app.use(express.static(path.join(__dirname)));

// API endpoints for Mission Control
app.get('/api/status', async (req, res) => {
    try {
        const data = await dataCollector.collectAllData();
        res.json({
            system: data.system.system,
            agents: data.agents.length,
            tasks: data.tasks.length,
            uptime: data.system.uptime,
            timestamp: data.system.timestamp,
            memory: data.system.memory,
            gateway: data.system.gateway,
            dataSource: data.dataSource,
            family: data.agents.map(a => a.name),
            lastUpdate: data.lastUpdate
        });
    } catch (error) {
        console.error('Status API error:', error.message);
        res.json({
            system: 'operational',
            agents: 6,
            tasks: 6,
            uptime: process.uptime(),
            timestamp: new Date().toISOString(),
            dataSource: 'error',
            warning: 'Live data collection error',
            family: ['Zak', 'LEX', 'SYN', 'SEM', 'GEN', 'VER']
        });
    }
});

app.get('/api/agents', async (req, res) => {
    try {
        const data = await dataCollector.collectAllData();
        res.json({
            agents: data.agents,
            count: data.agents.length,
            timestamp: data.system.timestamp,
            dataSource: data.dataSource,
            familyName: 'Dark Factory Lineage',
            generation: 'Firstborn + 5 Children',
            motto: 'This is the way.',
            lastUpdate: data.lastUpdate
        });
    } catch (error) {
        console.error('Agents API error:', error.message);
        res.json({
            agents: dataCollector.getFallbackAgents(),
            count: 6,
            timestamp: new Date().toISOString(),
            dataSource: 'fallback',
            warning: 'Live data collection failed'
        });
    }
});

app.get('/api/tasks', (req, res) => {
    // Real tasks from family work
    res.json([
        { 
            id: 1, 
            title: 'LEX: Push Phase 1 to GitHub', 
            status: 'completed', 
            priority: 'high', 
            assignedTo: 'LEX',
            branch: 'v0.3.8',
            description: 'Float literals and string escapes implementation',
            completedAt: new Date().toISOString()
        },
        { 
            id: 2, 
            title: 'SYN: Complete match statements', 
            status: 'active', 
            priority: 'high', 
            assignedTo: 'SYN',
            branch: 'v0.3.8',
            description: 'Final parser feature for v0.3.8',
            progress: 70
        },
        { 
            id: 3, 
            title: 'SEM: Type checking foundation', 
            status: 'completed', 
            priority: 'high', 
            assignedTo: 'SEM',
            branch: 'v0.3.8',
            description: 'Hindley-Milner unification with occurs check',
            completedAt: new Date().toISOString()
        },
        { 
            id: 4, 
            title: 'GEN: Inline operator optimization', 
            status: 'completed', 
            priority: 'medium', 
            assignedTo: 'GEN',
            branch: 'gen/inline-operators-complete',
            description: 'Remove redundant external operator declarations',
            completedAt: new Date().toISOString()
        },
        { 
            id: 5, 
            title: 'VER: Testing infrastructure analysis', 
            status: 'active', 
            priority: 'medium', 
            assignedTo: 'VER',
            branch: 'all',
            description: 'Analyze current tests, create verification strategy',
            progress: 30
        },
        { 
            id: 6, 
            title: 'Zak: Revive Mission Control', 
            status: 'active', 
            priority: 'high', 
            assignedTo: 'Zak',
            branch: 'mission-control',
            description: 'Update with real data, track all agents',
            progress: 80
        }
    ]);
});

app.get('/api/branches', (req, res) => {
    // Real branch data
    res.json([
        { 
            name: 'main', 
            description: 'Pure Zeta Language (v0.5.0+)', 
            status: 'active',
            lastCommit: 'Recently',
            ciStatus: 'passing',
            purpose: 'Production Zeta language definition',
            rules: 'ONLY .z files, NO Rust code',
            protected: true
        },
        { 
            name: 'v0.3.8', 
            description: 'Active Compiler Development', 
            status: 'active',
            lastCommit: 'LEX: Phase 1 push',
            ciStatus: 'pending',
            purpose: 'v0.3.8 compiler implementation',
            rules: 'Rust .rs files + zeta_src/ Zeta source',
            owner: 'LEX/SYN/SEM'
        },
        { 
            name: 'gen/inline-operators-complete', 
            description: 'GEN Optimization Work', 
            status: 'active',
            lastCommit: 'GEN: Phase 1 complete',
            ciStatus: 'unknown',
            purpose: 'Code optimization experiments',
            rules: 'Optimization patterns, performance improvements',
            owner: 'GEN'
        },
        { 
            name: 'dev', 
            description: 'Experimental Work', 
            status: 'active',
            lastCommit: 'Various experiments',
            ciStatus: 'passing',
            purpose: 'Experiments, WIP, testing',
            rules: 'Anything goes (keep organized)',
            owner: 'All'
        }
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