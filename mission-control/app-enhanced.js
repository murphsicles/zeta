// Enhanced OpenClaw Mission Control - Main Application
class EnhancedMissionControl {
    constructor() {
        this.currentTab = 'dashboard';
        this.ws = null;
        this.wsConnected = false;
        this.lastUpdate = Date.now();
        this.initialize();
    }

    initialize() {
        this.setupEventListeners();
        this.updateTime();
        this.connectWebSocket();
        this.loadInitialData();
        
        // Update time every second
        setInterval(() => this.updateTime(), 1000);
        
        // Auto-refresh data every 30 seconds
        setInterval(() => this.refreshData(), 30000);
    }

    setupEventListeners() {
        // Tab switching
        document.querySelectorAll('.tab').forEach(tab => {
            tab.addEventListener('click', (e) => {
                const tabName = e.target.dataset.tab;
                this.showTab(tabName);
            });
        });

        // Refresh buttons
        document.getElementById('refresh-agents')?.addEventListener('click', () => this.loadAgents());
        document.getElementById('refresh-github')?.addEventListener('click', () => this.loadGitHubData());
        document.getElementById('refresh-zeta')?.addEventListener('click', () => this.loadZetaProgress());
        document.getElementById('refresh-notifications')?.addEventListener('click', () => this.loadNotifications());
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
            'dashboard': 'Enhanced Mission Control',
            'agents': 'Agent Management',
            'repository': 'GitHub Repository',
            'zeta': 'Zeta Progress Dashboard',
            'notifications': 'System Notifications',
            'management': 'Agent Management'
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
            // Create dynamic content
            this.createDynamicTabContent(tabName);
        }

        this.currentTab = tabName;
        
        // Load tab-specific data
        switch(tabName) {
            case 'agents':
                this.loadAgents();
                break;
            case 'repository':
                this.loadGitHubData();
                break;
            case 'zeta':
                this.loadZetaProgress();
                break;
            case 'notifications':
                this.loadNotifications();
                break;
            case 'management':
                this.loadManagementInterface();
                break;
        }
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
            case 'agents':
                content.innerHTML = this.createAgentsContent();
                break;
            case 'repository':
                content.innerHTML = this.createRepositoryContent();
                break;
            case 'zeta':
                content.innerHTML = this.createZetaContent();
                break;
            case 'notifications':
                content.innerHTML = this.createNotificationsContent();
                break;
            case 'management':
                content.innerHTML = this.createManagementContent();
                break;
            default:
                content.innerHTML = `<div class="card"><div class="card-title">${tabName}</div><div class="card-content">Content for ${tabName} coming soon...</div></div>`;
        }

        mainContent.appendChild(content);
    }

    createAgentsContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🤖 Live Agents</span>
                        <button class="btn btn-primary" id="refresh-agents">Refresh</button>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="agents-detail-loading">Loading agent details...</div>
                        <div class="agent-grid" id="agents-detail-grid"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>📊 Agent Statistics</span>
                        <span class="badge badge-primary">Real-time</span>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Total Agents</span>
                            <span class="status-value" id="total-agents">0</span>
                        </div>
                        <div class="status-item">
                            <span>Online</span>
                            <span class="status-value" id="online-agents">0</span>
                        </div>
                        <div class="status-item">
                            <span>Busy</span>
                            <span class="status-value" id="busy-agents">0</span>
                        </div>
                        <div class="status-item">
                            <span>Total Tokens</span>
                            <span class="status-value" id="agents-total-tokens">0</span>
                        </div>
                        
                        <div style="margin-top: 20px;">
                            <h4 style="color: var(--accent); margin-bottom: 10px;">Recent Sessions</h4>
                            <div id="recent-sessions"></div>
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>🔗 OpenClaw Status</span>
                        <button class="btn" onclick="missionControl.testOpenClawConnection()">Test Connection</button>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Gateway URL</span>
                            <span class="status-value">localhost:18789</span>
                        </div>
                        <div class="status-item">
                            <span>Connection</span>
                            <span class="status-value" id="openclaw-connection-status">
                                <span class="connection-status disconnected"></span>
                                Testing...
                            </span>
                        </div>
                        <div class="status-item">
                            <span>Last Health Check</span>
                            <span class="status-value" id="last-health-check">Never</span>
                        </div>
                        <div class="status-item">
                            <span>WebSocket</span>
                            <span class="status-value" id="ws-connection-status">
                                <span class="connection-status disconnected"></span>
                                Disconnected
                            </span>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    createRepositoryContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🐙 Repository Overview</span>
                        <button class="btn btn-primary" id="refresh-github">Refresh</button>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="repo-loading">Loading repository data...</div>
                        <div id="repo-content" style="display: none;">
                            <div class="status-item">
                                <span>Repository</span>
                                <span class="status-value" id="repo-full-name">darkfactoryai/zeta</span>
                            </div>
                            <div class="status-item">
                                <span>Description</span>
                                <span class="status-value" id="repo-description">Loading...</span>
                            </div>
                            <div class="status-item">
                                <span>Stars</span>
                                <span class="status-value" id="repo-stars">0</span>
                            </div>
                            <div class="status-item">
                                <span>Forks</span>
                                <span class="status-value" id="repo-forks">0</span>
                            </div>
                            <div class="status-item">
                                <span>Last Updated</span>
                                <span class="status-value" id="repo-updated">Never</span>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>📝 Recent Commits</span>
                        <span class="badge badge-success">Live</span>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="commits-loading">Loading commits...</div>
                        <div id="commits-list" style="display: none;"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>⚡ CI/CD Status</span>
                        <button class="btn" onclick="missionControl.loadWorkflows()">Check Workflows</button>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="workflows-loading">Loading workflows...</div>
                        <div id="workflows-list" style="display: none;"></div>
                    </div>
                </div>
            </div>
        `;
    }

    createZetaContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🚀 Zeta Bootstrap Progress</span>
                        <button class="btn btn-primary" id="refresh-zeta">Refresh</button>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="zeta-loading">Loading Zeta progress...</div>
                        <div id="zeta-content" style="display: none;">
                            <div class="status-item">
                                <span>Current Version</span>
                                <span class="status-value" id="zeta-current-version">v0.3.7</span>
                            </div>
                            <div class="status-item">
                                <span>Target Version</span>
                                <span class="status-value" id="zeta-target-version">v1.0.0</span>
                            </div>
                            <div class="status-item">
                                <span>Overall Progress</span>
                                <span class="status-value" id="zeta-overall-progress">45%</span>
                            </div>
                            <div class="progress-bar">
                                <div class="progress-fill" id="zeta-progress-fill" style="width: 45%"></div>
                            </div>
                            
                            <h4 style="color: var(--accent); margin: 20px 0 10px 0;">Version Timeline</h4>
                            <div id="version-timeline"></div>
                        </div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>🎯 Current Blockers</span>
                        <span class="badge badge-danger" id="blocker-count">0</span>
                    </div>
                    <div class="card-content">
                        <div id="blockers-list"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>📈 Progress Metrics</span>
                        <span class="badge badge-primary">Live</span>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Commits to v0.3.8</span>
                            <span class="status-value" id="commits-to-next">0/50</span>
                        </div>
                        <div class="status-item">
                            <span>Tests Passing</span>
                            <span class="status-value" id="tests-passing">0%</span>
                        </div>
                        <div class="status-item">
                            <span>Documentation</span>
                            <span class="status-value" id="docs-progress">0%</span>
                        </div>
                        <div class="status-item">
                            <span>Code Coverage</span>
                            <span class="status-value" id="coverage">0%</span>
                        </div>
                        
                        <div style="margin-top: 20px;">
                            <h4 style="color: var(--accent); margin-bottom: 10px;">Next Release</h4>
                            <div id="next-release-details">Loading...</div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    createNotificationsContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🔔 System Notifications</span>
                        <button class="btn btn-primary" id="refresh-notifications">Refresh</button>
                    </div>
                    <div class="card-content">
                        <div class="loading" id="notifications-loading">Loading notifications...</div>
                        <div id="notifications-list"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>⚠️ Critical Alerts</span>
                        <span class="badge badge-danger" id="critical-alerts">0</span>
                    </div>
                    <div class="card-content">
                        <div id="critical-alerts-list"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>⚙️ Notification Settings</span>
                        <button class="btn" onclick="missionControl.showNotificationSettings()">Configure</button>
                    </div>
                    <div class="card-content">
                        <div class="status-item">
                            <span>Email Alerts</span>
                            <span class="status-value">Disabled</span>
                        </div>
                        <div class="status-item">
                            <span>Sound Alerts</span>
                            <span class="status-value">Enabled</span>
                        </div>
                        <div class="status-item">
                            <span>Desktop Notifications</span>
                            <span class="status-value">Enabled</span>
                        </div>
                        <div class="status-item">
                            <span>Check Frequency</span>
                            <span class="status-value">Every 30s</span>
                        </div>
                        
                        <div style="margin-top: 20px;">
                            <h4 style="color: var(--accent); margin-bottom: 10px;">Alert Types</h4>
                            <div>
                                <input type="checkbox" id="alert-ci" checked> CI Failures<br>
                                <input type="checkbox" id="alert-agent" checked> Agent Crashes<br>
                                <input type="checkbox" id="alert-commit" checked> New Commits<br>
                                <input type="checkbox" id="alert-milestone" checked> Milestones
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }

    createManagementContent() {
        return `
            <div class="dashboard-grid">
                <div class="card">
                    <div class="card-title">
                        <span>🤖 Spawn New Agent</span>
                        <span class="badge badge-primary">Experimental</span>
                    </div>
                    <div class="card-content">
                        <div style="margin-bottom: 15px;">
                            <label style="display: block; margin-bottom: 5px; color: var(--text);">Agent Name</label>
                            <input type="text" id="new-agent-name" placeholder="e.g., Documentation Specialist" style="width: 100%; padding: 8px; background: #222; border: 1px solid #333; color: white; border-radius: 4px;">
                        </div>
                        
                        <div style="margin-bottom: 15px;">
                            <label style="display: block; margin-bottom: 5px; color: var(--text);">Task Description</label>
                            <textarea id="new-agent-task" placeholder="Describe what this agent should do..." rows="3" style="width: 100%; padding: 8px; background: #222; border: 1px solid #333; color: white; border-radius: 4px; resize: vertical;"></textarea>
                        </div>
                        
                        <div style="margin-bottom: 15px;">
                            <label style="display: block; margin-bottom: 5px; color: var(--text);">Agent Type</label>
                            <select id="new-agent-type" style="width: 100%; padding: 8px; background: #222; border: 1px solid #333; color: white; border-radius: 4px;">
                                <option value="worker">Worker (Task-focused)</option>
                                <option value="specialist">Specialist (Domain expert)</option>
                                <option value="monitor">Monitor (Observability)</option>
                                <option value="analyst">Analyst (Data processing)</option>
                            </select>
                        </div>
                        
                        <button class="btn btn-primary" style="width: 100%;" onclick="missionControl.spawnAgent()">Spawn Agent</button>
                        
                        <div id="spawn-result" style="margin-top: 15px;"></div>
                    </div>
                </div>

                <div class="card">
                    <div class="card-title">
                        <span>📨 Send Message to Agent</span>
                        <button class="btn" onclick="missionControl.loadAgentsForMessaging()">Load Agents</button>
                    </div>
                    <div class="card-content">
                        <div style="margin-bottom: 15px;">
                            <label style="display: block; margin-bottom: 5px; color: var(--text);">Select Agent</label>
                            <select id="message-agent-select" style="width: 100%; padding: 8px; background: #222; border: 1px solid #333; color: white; border-radius: 4px;">
                                <option value="">-- Select Agent --</option>
                            </select>
                        </div>
                        
                        <div style="margin-bottom: 15px;">
                            <label style="display: block; margin-bottom: 5px; color: var(--text);">Message</label>
                            <textarea id="agent-message" placeholder="Type your message here..."