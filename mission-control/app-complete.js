// Complete the MissionControl class with all methods

// Add these methods to the existing MissionControl class in app.js:

    startLiveUpdates() {
        // Connect to WebSocket for real-time updates
        this.ws = new WebSocket(`ws://${window.location.host}`);
        
        this.ws.onopen = () => {
            console.log('Connected to Mission Control server');
            this.addActivity('WebSocket connected', 'System');
        };

        this.ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.handleWebSocketMessage(data);
        };

        this.ws.onclose = () => {
            console.log('Disconnected from server');
            setTimeout(() => {
                this.startLiveUpdates(); // Reconnect
            }, 5000);
        };
    }

    handleWebSocketMessage(data) {
        switch(data.type) {
            case 'welcome':
                console.log('Server:', data.message);
                break;
            case 'update':
                this.updateLiveData(data.data);
                break;
            case 'activity':
                this.addActivity(data.data.action, data.data.agent);
                break;
        }
    }

    updateLiveData(data) {
        document.getElementById('system-status').textContent = data.systemStatus === 'operational' ? '🟢 OPERATIONAL' : '🔴 OFFLINE';
        document.getElementById('active-agents').textContent = data.activeAgents;
        document.getElementById('pending-tasks').textContent = data.pendingTasks;
        document.getElementById('memory-usage').textContent = data.memoryUsage;
        
        // Update uptime if provided
        if (data.uptime) {
            const hours = Math.floor(data.uptime / 3600);
            const minutes = Math.floor((data.uptime % 3600) / 60);
            document.getElementById('uptime').textContent = `${hours}h ${minutes}m`;
        }
    }

    addActivity(action, agent) {
        const now = new Date();
        const timeString = now.toLocaleTimeString('en-GB', { 
            hour12: false,
            hour: '2-digit',
            minute: '2-digit'
        });
        
        const activity = {
            time: timeString,
            action: action,
            agent: agent
        };
        
        this.activities.unshift(activity);
        if (this.activities.length > 10) {
            this.activities.pop();
        }
        
        this.updateActivityFeed();
    }

    updateActivityFeed() {
        const feed = document.getElementById('activity-feed');
        if (feed) {
            feed.innerHTML = this.activities.map(activity => `
                <div class="timeline-item">
                    <div class="timeline-dot"></div>
                    <div class="timeline-time">${activity.time}</div>
                    <div class="timeline-content">${activity.action} <small>(${activity.agent})</small></div>
                </div>
            `).join('');
        }
    }

    addSampleTask() {
        const newTask = {
            title: `New Task ${this.tasks.length + 1}`,
            description: 'Sample task description',
            assignedTo: 'Dark Factory',
            status: 'pending'
        };
        
        this.tasks.push(newTask);
        this.showTab('tasks'); // Refresh tasks view
        this.addActivity(`Added new task: ${newTask.title}`, 'User');
    }

    addSampleAgent() {
        const roles = ['Assistant', 'Specialist', 'Monitor', 'Worker', 'Analyst'];
        const avatars = ['🤖', '⚙️', '📊', '🔧', '🧠', '🔍', '📈', '💾'];
        const statuses = ['online', 'busy', 'offline'];
        
        const newAgent = {
            name: `Agent ${this.agents.length + 1}`,
            role: roles[Math.floor(Math.random() * roles.length)],
            status: statuses[Math.floor(Math.random() * statuses.length)],
            avatar: avatars[Math.floor(Math.random() * avatars.length)]
        };
        
        this.agents.push(newAgent);
        this.showTab('agents'); // Refresh agents view
        this.addActivity(`Added new agent: ${newAgent.name}`, 'User');
    }

    createNewAgent() {
        const name = document.getElementById('new-agent-name').value;
        const role = document.getElementById('new-agent-role').value;
        
        if (!name) {
            alert('Please enter agent name');
            return;
        }
        
        const avatars = {
            assistant: '🤖',
            specialist: '🔧',
            monitor: '📊',
            worker: '⚙️',
            analyst: '🧠'
        };
        
        const newAgent = {
            name: name,
            role: role.charAt(0).toUpperCase() + role.slice(1),
            status: 'online',
            avatar: avatars[role] || '🤖'
        };
        
        this.agents.push(newAgent);
        
        // Clear form
        document.getElementById('new-agent-name').value = '';
        
        this.showTab('agents');
        this.addActivity(`Created new ${role} agent: ${name}`, 'User');
    }

    messageAgent(agentName) {
        const message = prompt(`Message to ${agentName}:`);
        if (message) {
            this.addActivity(`Sent message to ${agentName}: "${message}"`, 'User');
            // In real implementation, this would send via OpenClaw API
        }
    }

    configureAgent(agentName) {
        alert(`Configure ${agentName}\n\nIn full implementation, this would open agent configuration panel with:\n- Model settings\n- Capabilities\n- Memory configuration\n- Task assignments\n- Behavior tuning`);
    }

    testConnections() {
        this.addActivity('Testing all agent connections...', 'System');
        
        // Simulate connection tests
        setTimeout(() => {
            const onlineCount = this.agents.filter(a => a.status === 'online' || a.status === 'busy').length;
            this.addActivity(`Connection test complete: ${onlineCount}/${this.agents.length} agents online`, 'System');
        }, 2000);
    }

    refreshData() {
        this.addActivity('Manual refresh requested', 'User');
        
        // Simulate API call
        fetch('/api/status')
            .then(response => response.json())
            .then(data => {
                this.updateLiveData(data);
                this.addActivity('Data refreshed from server', 'System');
            })
            .catch(error => {
                console.error('Refresh error:', error);
                this.addActivity('Refresh failed - using cached data', 'System');
            });
    }

// Initialize the app when page loads
document.addEventListener('DOMContentLoaded', () => {
    window.missionControl = new MissionControl();
    
    // Add refresh button to header
    const header = document.querySelector('.header');
    const refreshBtn = document.createElement('button');
    refreshBtn.className = 'btn';
    refreshBtn.id = 'refresh-btn';
    refreshBtn.textContent = '🔄 Refresh';
    refreshBtn.style.marginLeft = '10px';
    header.appendChild(refreshBtn);
    
    // Initialize activity feed
    missionControl.activities = [
        { time: '01:45', action: 'Mission Control started', agent: 'System' },
        { time: '01:46', action: 'Dark Factory connected', agent: 'Dark Factory' },
        { time: '01:47', action: 'Bootstrap Worker started job', agent: 'Bootstrap Worker' },
        { time: '01:50', action: 'GitHub CI fixed', agent: 'CI Monitor' },
        { time: '01:55', action: 'Building Mission Control app', agent: 'Dark Factory' }
    ];
    missionControl.updateActivityFeed();
});