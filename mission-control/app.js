// OpenClaw Mission Control - Main Application
class MissionControl {
    constructor() {
        this.currentTab = 'dashboard';
        this.activities = [];
        this.agents = [];
        this.tasks = [];
        this.initialize();
    }

    initialize() {
        this.setupEventListeners();
        this.updateTime();
        this.loadSampleData();
        this.startLiveUpdates();
        
        // Update status every 30 seconds
        setInterval(() => this.updateStatus(), 30000);
        
        // Update time every second
        setInterval(() => this.updateTime(), 1000);
    }

    setupEventListeners() {
        // Tab switching
        document.querySelectorAll('.tab').forEach(tab => {
            tab.addEventListener('click', (e) => {
                const tabName = e.target.dataset.tab;
                this.showTab(tabName);
            });
        });

        // Add sample buttons
        document.getElementById('add-task-btn')?.addEventListener('click', () => this.addSampleTask());
        document.getElementById('add-agent-btn')?.addEventListener('click', () => this.addSampleAgent());
        document.getElementById('refresh-btn')?.addEventListener('click', () => this.refreshData());
    }

    showTab(tabName) {
        // Update active tab in sidebar
        document.querySelectorAll('.tab').forEach(tab => {
            tab.classList.remove('active');
            if (tab.dataset.tab === tabName) {
                tab.classList.add('active');
            }
        });

        // Update page title
        const tabTitles = {
            'dashboard': 'Mission Control Dashboard',
            'tasks': 'Task Management',
            'agents': 'Agent Coordination',
            'content': 'Content Repository',
            'approvals': 'Approval Queue',
            'council': 'AI Council',
            'calendar': 'Schedule & Calendar',
            'projects': 'Project Management',
            'memory': 'Agent Memory',
            'office': 'Workspace Office',
            'team': 'Team Structure',
            'factory': 'Dark Factory',
            'pipeline': 'Build Pipeline',
            'ai-lab': 'AI Laboratory',
            'feedback': 'Feedback & Tuning'
        };

        document.getElementById('page-title').textContent = tabTitles[tabName] || tabName;

        // Show/hide tab content
        document.querySelectorAll('.tab-content').forEach(content => {
            content.classList.remove('active');
        });
        
        const tabContent = document.getElementById(`${tabName}-content`);
        if (tabContent) {
            tabContent.classList.add('active');
        } else {
            // Create dynamic content for tabs that don't have static HTML
            this.createDynamicTabContent(tabName);
        }

        this.currentTab = tabName;
    }

    createDynamicTabContent(tabName) {
        const mainContent = document.querySelector('.main-content');
        
        // Remove existing dynamic content
        const existingDynamic = document.querySelector('.tab-content.dynamic');
        if (existingDynamic) {
            existingDynamic.remove();
        }

        // Create new dynamic content
        const content = document.createElement('div');
        content.className = `tab-content dynamic active`;
        content.id = `${tabName}-content`;

        switch(tabName) {
            case 'tasks':
                content.innerHTML = this.createTasksContent();
                break;
            case 'agents':
                content.innerHTML = this.createAgentsContent();
                break;
            case 'content':
                content.innerHTML = this.createContentContent();
                break;
            case 'approvals':
                content.innerHTML = this.createApprovalsContent();
                break;
            case 'council':
                content.innerHTML = this.createCouncilContent();
                break;
            case 'calendar':
                content.innerHTML = this.createCalendarContent();
                break;
            case 'projects':
                content.innerHTML = this.createProjectsContent();
                break;
            case 'memory':
                content.innerHTML = this.createMemoryContent();
                break;
            case 'office':
                content.innerHTML = this.createOfficeContent();
                break;
            case 'team':
                content.innerHTML = this.createTeamContent();
                break;
            case 'factory':
                content.innerHTML = this.createFactoryContent();
                break;
            case 'pipeline':
                content.innerHTML = this.createPipelineContent();
                break;
            case 'ai-lab':
                content.innerHTML = this.createAILabContent();
                break;
            case 'feedback':
                content.innerHTML = this.createFeedbackContent();
                break;
            default:
                content.innerHTML = `<div class="card"><div class="card-title">${tabName}</div><div class="card-content">Content for ${tabName} coming soon...</div></div>`;
        }

        mainContent.appendChild(content);
    }

    createTasksContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>📋 Task Queue</span>
                        <button class="btn btn-primary" id="add-task-btn">+ Add Task</button>
                    </div>
                    <div class="card-content">
                        <div class="task-list" id="task-list">
                            ${this.tasks.map(task => `
                                <div class="task-item">
                                    <div class="task-info">
                                        <h4>${task.title}</h4>
                                        <p>${task.description}</p>
                                        <small>Assigned to: ${task.assignedTo}</small>
                                    </div>
                                    <span class="task-status status-${task.status}">${task.status.toUpperCase()}</span>
                                </div>
                            `).join('')}
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>📊 Task Statistics</span>
                        <span class="badge badge-primary">Real-time</span>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Total Tasks</span>
                            <span class="status-value">${this.tasks.length}</span>
                        </div>
                        <div class="status-item">
                            <span>Active</span>
                            <span class="status-value">${this.tasks.filter(t => t.status === 'active').length}</span>
                        </div>
                        <div class="status-item">
                            <span>Completed Today</span>
                            <span class="status-value">${this.tasks.filter(t => t.status === 'completed').length}</span>
                        </div>
                        <div class="status-item">
                            <span>Blocked</span>
                            <span class="status-value">${this.tasks.filter(t => t.status === 'blocked').length}</span>
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>🎯 Priority Tasks</span>
                        <button class="btn" onclick="missionControl.refreshData()">Refresh</button>
                    </div>
                    <div class="card-content">
                        <div class="task-list">
                            <div class="task-item">
                                <div class="task-info">
                                    <h4>Fix v0.3.7 Const Parsing</h4>
                                    <p>Add ConstDef to AST, parse_const function in Rust source</p>
                                    <small>Project: Zeta Bootstrap • Due: Today</small>
                                </div>
                                <span class="task-status status-active">HIGH</span>
                            </div>
                            <div class="task-item">
                                <div class="task-info">
                                    <h4>Build Mission Control v2</h4>
                                    <p>Add real-time WebSocket connections to OpenClaw</p>
                                    <small>Project: Infrastructure • Due: Tomorrow</small>
                                </div>
                                <span class="task-status status-pending">MEDIUM</span>
                            </div>
                            <div class="task-item">
                                <div class="task-info">
                                    <h4>Setup Zeta Foundation AI Council</h4>
                                    <p>Create specialized agents for different roles</p>
                                    <small>Project: Organization • Due: This week</small>
                                </div>
                                <span class="task-status status-pending">MEDIUM</span>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    createAgentsContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🤖 Active Agents</span>
                        <button class="btn btn-primary" id="add-agent-btn">+ Add Agent</button>
                    </div>
                    <div class="card-content">
                        <div class="agent-grid">
                            ${this.agents.map(agent => `
                                <div class="agent-card">
                                    <div class="agent-avatar">${agent.avatar}</div>
                                    <div class="agent-name">${agent.name}</div>
                                    <div class="agent-role">${agent.role}</div>
                                    <div>
                                        <span class="agent-status status-${agent.status}"></span>
                                        <span>${agent.status}</span>
                                    </div>
                                    <div style="margin-top: 10px;">
                                        <button class="btn" style="font-size: 10px; padding: 4px 8px;" onclick="missionControl.messageAgent('${agent.name}')">Message</button>
                                        <button class="btn" style="font-size: 10px; padding: 4px 8px;" onclick="missionControl.configureAgent('${agent.name}')">Configure</button>
                                    </div>
                                </div>
                            `).join('')}
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>⚙️ Agent Configuration</span>
                        <span class="badge badge-success">Live</span>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Total Agents</span>
                            <span class="status-value">${this.agents.length}</span>
                        </div>
                        <div class="status-item">
                            <span>Online</span>
                            <span class="status-value">${this.agents.filter(a => a.status === 'online').length}</span>
                        </div>
                        <div class="status-item">
                            <span>Busy</span>
                            <span class="status-value">${this.agents.filter(a => a.status === 'busy').length}</span>
                        </div>
                        <div class="status-item">
                            <span>Available</span>
                            <span class="status-value">${this.agents.filter(a => a.status === 'online' && !a.busy).length}</span>
                        </div>
                        
                        <div style="margin-top: 20px;">
                            <h4 style="color: var(--accent); margin-bottom: 10px;">Create New Agent</h4>
                            <input type="text" id="new-agent-name" placeholder="Agent Name" style="width: 100%; padding: 8px; margin-bottom: 10px; background: #222; border: 1px solid #333; color: white; border-radius: 4px;">
                            <select id="new-agent-role" style="width: 100%; padding: 8px; margin-bottom: 10px; background: #222; border: 1px solid #333; color: white; border-radius: 4px;">
                                <option value="assistant">Assistant</option>
                                <option value="specialist">Specialist</option>
                                <option value="monitor">Monitor</option>
                                <option value="worker">Worker</option>
                                <option value="analyst">Analyst</option>
                            </select>
                            <button class="btn btn-primary" style="width: 100%;" onclick="missionControl.createNewAgent()">Create Agent</button>
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>🔗 Agent Connections</span>
                        <button class="btn" onclick="missionControl.testConnections()">Test All</button>
                    </div>
                    <div class="card-content">
                        <div class="timeline">
                            <div class="timeline-item">
                                <div class="timeline-dot"></div>
                                <div class="timeline-time">Just now</div>
                                <div class="timeline-content">Dark Factory connected to OpenClaw API</div>
                            </div>
                            <div class="timeline-item">
                                <div class="timeline-dot"></div>
                                <div class="timeline-time">5 min ago</div>
                                <div class="timeline-content">Bootstrap Worker started compilation job</div>
                            </div>
                            <div class="timeline-item">
                                <div class="timeline-dot"></div>
                                <div class="timeline-time">15 min ago</div>
                                <div class="timeline-content">CI Monitor detected GitHub push</div>
                            </div>
                            <div class="timeline-item">
                                <div class="timeline-dot"></div>
                                <div class="timeline-time">30 min ago</div>
                                <div class="timeline-content">All agents heartbeat check passed</div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    // Additional content creators for other tabs would go here...
    // For brevity, I'll create simplified versions

    createContentContent() {
        return `
            <div class="card">
                <div class="card-title">📝 Content Repository</div>
                <div class="card-content">
                    <p>All generated content, documents, and code organized here.</p>
                    <div class="task-list">
                        <div class="task-item">
                            <div class="task-info">
                                <h4>Zeta v0.3.8 Source Code</h4>
                                <p>Modified Rust AST and parser for const support</p>
                                <small>Type: Code • Size: 15KB • Created: Today</small>
                            </div>
                            <button class="btn">View</button>
                        </div>
                        <div class="task-item">
                            <div class="task-info">
                                <h4>Mission Control Documentation</h4>
                                <p>Architecture and API documentation</p>
                                <small>Type: Docs • Size: 8KB • Created: Now</small>
                            </div>
                            <button class="btn">Edit</button>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    createApprovalsContent() {
        return `
            <div class="card">
                <div class="card-title">🔄 Approval Queue</div>
                <div class="card-content">
                    <p>Pending decisions requiring human approval.</p>
                    <div class="task-list">
                        <div class="task-item">
                            <div class="task-info">
                                <h4>Force Push to Main Branch</h4>
                                <p>Required to fix CI configuration</p>
                                <small>Risk: High • Requested by: Dark Factory</small>
                            </div>
                            <div>
                                <button class="btn btn-primary" style="margin-right: 5px;">Approve</button>
                                <button class="btn" style="background: var(--danger);">Deny</button>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    updateTime() {
        const now = new Date();
        const timeString = now.toLocaleTimeString('en-GB', { 
            hour12: false,
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit'
        });
        const dateString = now.toLocaleDateString('en-GB', {
            weekday: 'long',
            year: 'numeric',
            month: 'long',
            day: 'numeric'
        });
        
        document.getElementById('current-time').textContent = `${dateString} • ${timeString} GMT`;
    }

    updateStatus() {
        // Simulate status updates
        const activeAgents = this.agents.filter(a => a.status === 'online' || a.status === 'busy').length;
        const pendingTasks = this.tasks.filter(t => t.status === 'pending' || t.status === 'active').length;
        
        document.getElementById('active-agents').textContent = activeAgents;
        document.getElementById('pending-tasks').textContent = pendingTasks;
        document.getElementById('memory-usage').textContent = `${Math.floor(Math.random() * 30) + 30}%`;
        
        // Update last heartbeat
        const minutes = Math.floor(Math.random() * 5);
        document.getElementById('last-heartbeat').textContent = `${minutes} min ago`;
        document.getElementById('next-check').textContent = `${30 - minutes} min`;
    }

    loadSampleData() {
        // Sample agents
        this.agents = [
            { name: 'Dark Factory', role: 'Main Assistant', status: 'online', avatar: '🏭' },
            { name: 'Bootstrap Worker', role: '24/7 Compiler', status: 'busy', avatar: '⚙️' },
            { name: 'CI Monitor', role: 'Quality Assurance', status: 'online', avatar: '📊' },
            { name: 'Memory Keeper', role: 'Context Manager', status: 'online', avatar: '🧠' },
            { name: 'Git Guardian', role: 'Repository Manager', status: 'offline', avatar: '🔒' }
        ];

        // Sample tasks
        this.tasks = [
            { title: 'Fix v0.3.7 Const Parsing', description: 'Add ConstDef to AST, parse_const function', assignedTo: 'Dark Factory', status: 'active' },
            { title: 'Clean GitHub Repository', description: 'Remove token functions, organize structure', assignedTo: 'Git Guardian', status: 'completed' },
            { title: 'Build Mission Control', description: 'Create visual dashboard for OpenClaw', assignedTo: 'Dark Factory', status: 'active' },
            { title: 'Setup 24/7 Worker', description: 'Configure autonomous bootstrap compiler', assignedTo: 'Bootstrap Worker', status: 'completed' },
            { title: 'Create CI Workflows', description: 'Add rustfmt, clippy, multi-OS testing', assignedTo: 'CI Monitor', status: 'pending' }
        ];

        // Sample activities