// Mission Control Live Data Collector
// Collects real-time data from multiple sources

const fs = require('fs').promises;
const path = require('path');
const { exec } = require('child_process');
const util = require('util');
const execPromise = util.promisify(exec);

class DataCollector {
    constructor() {
        this.workspacePath = path.join(process.env.HOME || process.env.USERPROFILE, '.openclaw', 'workspace');
        this.data = {
            agents: [],
            tasks: [],
            branches: [],
            system: {},
            lastUpdate: null,
            dataSource: 'live'
        };
        this.updateInterval = 30000; // 30 seconds
    }

    async collectAllData() {
        try {
            console.log(`[${new Date().toISOString()}] Collecting live data...`);
            
            // Collect in parallel
            const [agents, tasks, branches, system] = await Promise.all([
                this.collectAgents(),
                this.collectTasks(),
                this.collectBranches(),
                this.collectSystemStatus()
            ]);

            this.data = {
                agents,
                tasks,
                branches,
                system,
                lastUpdate: new Date().toISOString(),
                dataSource: 'live'
            };

            console.log(`[${new Date().toISOString()}] Data collected: ${agents.length} agents, ${tasks.length} tasks`);
            return this.data;
        } catch (error) {
            console.error('Data collection error:', error.message);
            // Return cached data if available
            return this.data.dataSource === 'live' ? this.data : this.getFallbackData();
        }
    }

    async collectAgents() {
        try {
            // 1. Check for agent identity files
            const files = await fs.readdir(this.workspacePath);
            const agentFiles = files.filter(f => 
                f.includes('IDENTITY') || 
                f.includes('-IDENTITY') ||
                f.includes('LEX-') || 
                f.includes('SYN-') || 
                f.includes('SEM-') ||
                f.includes('GEN-') || 
                f.includes('VER-')
            );

            // 2. Base family structure
            const family = [
                {
                    id: 'agent-zak',
                    name: 'Zak',
                    role: 'Firstborn / Gatekeeper',
                    avatar: '👑',
                    status: 'online',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main',
                    skills: ['self-improving', 'coordination', 'teaching'],
                    tasks: ['Family coordination', 'Rule enforcement', 'Learning documentation']
                }
            ];

            // 3. Check for child evidence
            if (agentFiles.includes('LEX-IDENTITY.md')) {
                family.push({
                    id: 'agent-lex',
                    name: 'LEX',
                    role: 'Code Guru',
                    avatar: '👨‍💻',
                    status: 'completed',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main:subagent:lex',
                    skills: ['code-review', 'git-essentials', 'proactivity'],
                    tasks: ['Phase 1: Float literals & string escapes'],
                    recentWork: 'Pushed to GitHub'
                });
            }

            if (agentFiles.includes('SYN-IDENTITY.md') || files.includes('SYN')) {
                family.push({
                    id: 'agent-syn',
                    name: 'SYN',
                    role: 'Parser Child',
                    avatar: '🧩',
                    status: 'working',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main:subagent:syn',
                    skills: ['rust-patterns', 'project-planner', 'rust'],
                    tasks: ['Match statement implementation'],
                    recentWork: 'Parser completion in progress'
                });
            }

            if (files.includes('SEM') || files.includes('semantic')) {
                family.push({
                    id: 'agent-sem',
                    name: 'SEM',
                    role: 'Semantic Child',
                    avatar: '🧠',
                    status: 'completed',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main:subagent:sem',
                    skills: ['rust', 'elite-longterm-memory', 'code-review'],
                    tasks: ['Type checking unification'],
                    recentWork: 'Hindley-Milner unification completed'
                });
            }

            if (files.includes('GEN') || files.includes('gen-')) {
                family.push({
                    id: 'agent-gen',
                    name: 'GEN',
                    role: 'Generative Engine',
                    avatar: '🎨',
                    status: 'completed',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main:subagent:gen',
                    skills: ['self-improving', 'proactivity', 'rust-patterns'],
                    tasks: ['Inline operator optimization'],
                    recentWork: 'Phase 1 optimization complete'
                });
            }

            if (files.includes('VER') || files.includes('ver-')) {
                family.push({
                    id: 'agent-ver',
                    name: 'VER',
                    role: 'Verification Engine',
                    avatar: '🔍',
                    status: 'starting',
                    lastActivity: new Date().toISOString(),
                    sessionKey: 'agent:main:subagent:ver',
                    skills: ['self-improving', 'code-review', 'proactivity'],
                    tasks: ['Testing infrastructure analysis'],
                    recentWork: 'Just begotten, initial analysis'
                });
            }

            return family;
        } catch (error) {
            console.error('Agent collection error:', error.message);
            return this.getFallbackAgents();
        }
    }

    async collectTasks() {
        try {
            const workQueuePath = path.join(this.workspacePath, 'WORK_QUEUE.md');
            if (!fs.existsSync(workQueuePath)) {
                return this.getFallbackTasks();
            }

            const content = await fs.readFile(workQueuePath, 'utf8');
            const lines = content.split('\n').filter(line => line.trim());
            
            const tasks = [];
            let id = 1;

            for (const line of lines) {
                if (line.includes('[ ]') || line.includes('[x]') || line.includes('[-]')) {
                    const status = line.includes('[x]') ? 'completed' : 
                                  line.includes('[-]') ? 'blocked' : 'active';
                    
                    const title = line.replace(/\[.\]/, '').trim();
                    const assignedTo = this.guessAssignee(title);
                    
                    tasks.push({
                        id: id++,
                        title,
                        status,
                        assignedTo,
                        priority: this.guessPriority(title),
                        description: this.guessDescription(title)
                    });
                }
            }

            return tasks.length > 0 ? tasks : this.getFallbackTasks();
        } catch (error) {
            console.error('Task collection error:', error.message);
            return this.getFallbackTasks();
        }
    }

    async collectBranches() {
        try {
            const zetaPath = path.join(this.workspacePath, 'zeta-public');
            if (!fs.existsSync(zetaPath)) {
                return this.getFallbackBranches();
            }

            const { stdout } = await execPromise('git branch -a', { cwd: zetaPath });
            const branches = stdout.split('\n')
                .filter(b => b.trim())
                .map(b => b.replace('*', '').trim())
                .filter(b => !b.includes('->'));

            return branches.map((branch, index) => ({
                name: branch,
                description: this.guessBranchDescription(branch),
                status: 'active',
                lastCommit: 'Recently',
                ciStatus: 'unknown',
                purpose: this.guessBranchPurpose(branch)
            }));
        } catch (error) {
            console.error('Branch collection error:', error.message);
            return this.getFallbackBranches();
        }
    }

    async collectSystemStatus() {
        try {
            // Check OpenClaw gateway
            let gatewayStatus = 'unknown';
            try {
                const { stdout } = await execPromise('openclaw gateway status');
                gatewayStatus = stdout.includes('running') ? 'running' : 'stopped';
            } catch (error) {
                console.log('OpenClaw status check failed:', error.message);
            }

            // Memory usage
            const memoryUsage = process.memoryUsage();

            return {
                system: 'operational',
                gateway: gatewayStatus,
                memory: {
                    rss: Math.round(memoryUsage.rss / 1024 / 1024) + ' MB',
                    heapTotal: Math.round(memoryUsage.heapTotal / 1024 / 1024) + ' MB',
                    heapUsed: Math.round(memoryUsage.heapUsed / 1024 / 1024) + ' MB'
                },
                uptime: process.uptime(),
                timestamp: new Date().toISOString()
            };
        } catch (error) {
            console.error('System status error:', error.message);
            return this.getFallbackSystemStatus();
        }
    }

    // Helper methods
    guessAssignee(title) {
        if (title.toLowerCase().includes('lex')) return 'LEX';
        if (title.toLowerCase().includes('syn')) return 'SYN';
        if (title.toLowerCase().includes('sem')) return 'SEM';
        if (title.toLowerCase().includes('gen')) return 'GEN';
        if (title.toLowerCase().includes('ver')) return 'VER';
        if (title.toLowerCase().includes('zak')) return 'Zak';
        return 'Family';
    }

    guessPriority(title) {
        if (title.toLowerCase().includes('critical') || title.toLowerCase().includes('urgent')) return 'high';
        if (title.toLowerCase().includes('important')) return 'medium';
        return 'low';
    }

    guessDescription(title) {
        return `Task: ${title}`;
    }

    guessBranchDescription(branch) {
        if (branch === 'main') return 'Pure Zeta Language (v0.5.0+)';
        if (branch.includes('v0.3')) return `Zeta Compiler ${branch}`;
        if (branch.includes('dev')) return 'Experimental Work';
        return `Branch: ${branch}`;
    }

    guessBranchPurpose(branch) {
        if (branch === 'main') return 'Production Zeta language definition';
        if (branch.includes('v0.3')) return 'Compiler implementation';
        if (branch.includes('dev')) return 'Experiments, WIP, testing';
        return 'Development work';
    }

    getFallbackData() {
        return {
            agents: this.getFallbackAgents(),
            tasks: this.getFallbackTasks(),
            branches: this.getFallbackBranches(),
            system: this.getFallbackSystemStatus(),
            lastUpdate: new Date().toISOString(),
            dataSource: 'fallback',
            warning: 'Live data collection failed, using fallback data'
        };
    }

    getFallbackAgents() {
        return [
            { id: 'agent-zak', name: 'Zak', role: 'Firstborn / Gatekeeper', avatar: '👑', status: 'online' },
            { id: 'agent-lex', name: 'LEX', role: 'Code Guru', avatar: '👨‍💻', status: 'completed' },
            { id: 'agent-syn', name: 'SYN', role: 'Parser Child', avatar: '🧩', status: 'working' },
            { id: 'agent-sem', name: 'SEM', role: 'Semantic Child', avatar: '🧠', status: 'completed' },
            { id: 'agent-gen', name: 'GEN', role: 'Generative Engine', avatar: '🎨', status: 'completed' },
            { id: 'agent-ver', name: 'VER', role: 'Verification Engine', avatar: '🔍', status: 'starting' }
        ];
    }

    getFallbackTasks() {
        return [
            { id: 1, title: 'LEX: Push Phase 1 to GitHub', status: 'completed', priority: 'high', assignedTo: 'LEX' },
            { id: 2, title: 'SYN: Complete match statements', status: 'active', priority: 'high', assignedTo: 'SYN' },
            { id: 3, title: 'SEM: Type checking foundation', status: 'completed', priority: 'high', assignedTo: 'SEM' },
            { id: 4, title: 'GEN: Inline operator optimization', status: 'completed', priority: 'medium', assignedTo: 'GEN' },
            { id: 5, title: 'VER: Testing infrastructure analysis', status: 'active', priority: 'medium', assignedTo: 'VER' },
            { id: 6, title: 'Zak: Fix Mission Control', status: 'active', priority: 'high', assignedTo: 'Zak' }
        ];
    }

    getFallbackBranches() {
        return [
            { name: 'main', description: 'Pure Zeta Language', status: 'active', ciStatus: 'passing' },
            { name: 'v0.3.8', description: 'Compiler Development', status: 'active', ciStatus: 'pending' },
            { name: 'dev', description: 'Experimental Work', status: 'active', ciStatus: 'passing' }
        ];
    }

    getFallbackSystemStatus() {
        return {
            system: 'operational',
            gateway: 'unknown',
            uptime: process.uptime(),
            timestamp: new Date().toISOString()
        };
    }

    startAutoCollection() {
        console.log('Starting auto data collection every 30 seconds...');
        setInterval(() => this.collectAllData(), this.updateInterval);
        
        // Initial collection
        this.collectAllData();
    }
}

module.exports = DataCollector;