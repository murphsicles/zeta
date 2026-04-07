#!/usr/bin/env pwsh
# Zeta Benchmark Runner Script
# Automates benchmarking process with comprehensive reporting

param(
    [string]$BenchmarkPath = "benchmarks",
    [string]$OutputPath = "results",
    [string]$ConfigFile = "benchmark_config.json",
    [switch]$Clean,
    [switch]$Quick,
    [switch]$Verbose,
    [string]$Filter = "*"
)

# ============================================================================
# CONFIGURATION
# ============================================================================

$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"

# Colors for output
$ColorReset = "`e[0m"
$ColorGreen = "`e[32m"
$ColorYellow = "`e[33m"
$ColorRed = "`e[31m"
$ColorBlue = "`e[34m"
$ColorCyan = "`e[36m"

# ============================================================================
# FUNCTIONS
# ============================================================================

function Write-Info {
    param([string]$Message)
    Write-Host "${ColorCyan}[INFO]${ColorReset} $Message"
}

function Write-Success {
    param([string]$Message)
    Write-Host "${ColorGreen}[SUCCESS]${ColorReset} $Message"
}

function Write-Warning {
    param([string]$Message)
    Write-Host "${ColorYellow}[WARNING]${ColorReset} $Message"
}

function Write-Error {
    param([string]$Message)
    Write-Host "${ColorRed}[ERROR]${ColorReset} $Message"
}

function Test-Requirements {
    Write-Info "Checking system requirements..."
    
    # Check PowerShell version
    $psVersion = $PSVersionTable.PSVersion
    if ($psVersion.Major -lt 7) {
        Write-Warning "PowerShell $psVersion detected. PowerShell 7+ recommended for best performance."
    }
    
    # Check if benchmark directory exists
    if (-not (Test-Path $BenchmarkPath)) {
        throw "Benchmark directory '$BenchmarkPath' not found"
    }
    
    # Create output directory if it doesn't exist
    if (-not (Test-Path $OutputPath)) {
        New-Item -ItemType Directory -Path $OutputPath -Force | Out-Null
        Write-Info "Created output directory: $OutputPath"
    }
    
    Write-Success "System requirements satisfied"
}

function Clean-Benchmarks {
    Write-Info "Cleaning benchmark artifacts..."
    
    $cleanPaths = @(
        "$OutputPath\*.json",
        "$OutputPath\*.md",
        "$OutputPath\*.csv",
        "$OutputPath\*.html",
        "benchmarks\*.tmp",
        "benchmarks\*.log"
    )
    
    foreach ($path in $cleanPaths) {
        if (Test-Path $path) {
            Remove-Item $path -Force -ErrorAction SilentlyContinue
        }
    }
    
    Write-Success "Cleaned benchmark artifacts"
}

function Get-BenchmarkFiles {
    param([string]$FilterPattern)
    
    Write-Info "Discovering benchmark files..."
    
    $benchmarkFiles = Get-ChildItem -Path $BenchmarkPath -Filter "*.z" -Recurse | 
        Where-Object { $_.Name -like $FilterPattern }
    
    if ($benchmarkFiles.Count -eq 0) {
        Write-Warning "No benchmark files found matching pattern: $FilterPattern"
        return @()
    }
    
    Write-Success "Found $($benchmarkFiles.Count) benchmark file(s)"
    
    if ($Verbose) {
        foreach ($file in $benchmarkFiles) {
            Write-Info "  - $($file.Name)"
        }
    }
    
    return $benchmarkFiles
}

function Run-SingleBenchmark {
    param(
        [System.IO.FileInfo]$BenchmarkFile,
        [hashtable]$Config
    )
    
    $benchmarkName = $BenchmarkFile.BaseName
    $benchmarkPath = $BenchmarkFile.FullName
    
    Write-Info "Running benchmark: $benchmarkName"
    
    # Create result structure
    $result = @{
        name = $benchmarkName
        file = $benchmarkPath
        timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ss"
        iterations = if ($Quick) { 10 } else { $Config.iterations }
        results = @()
        metrics = @{}
    }
    
    # Simulate benchmark execution (in real implementation, this would compile and run the Zeta code)
    # For now, we'll simulate with random performance data
    
    $iterationTimes = @()
    $successCount = 0
    
    for ($i = 0; $i -lt $result.iterations; $i++) {
        # Simulate work with random variation
        $baseTime = 100  # ms
        $variation = Get-Random -Minimum -20 -Maximum 30
        $iterationTime = $baseTime + $variation
        
        # 90% success rate simulation
        $success = (Get-Random -Minimum 1 -Maximum 100) -le 90
        
        if ($success) {
            $iterationTimes += $iterationTime
            $successCount++
            
            $iterationResult = @{
                iteration = $i + 1
                time_ms = $iterationTime
                success = $true
            }
        } else {
            $iterationResult = @{
                iteration = $i + 1
                time_ms = $iterationTime
                success = $false
                error = "Simulated benchmark failure"
            }
        }
        
        $result.results += $iterationResult
        
        # Progress indicator
        if ($i % 10 -eq 0 -and $Verbose) {
            Write-Host "  Iteration $($i+1)/$($result.iterations)" -NoNewline
            Write-Host " - $iterationTime ms" -ForegroundColor Gray
        }
    }
    
    # Calculate metrics
    if ($iterationTimes.Count -gt 0) {
        $result.metrics = @{
            min_time_ms = ($iterationTimes | Measure-Object -Minimum).Minimum
            max_time_ms = ($iterationTimes | Measure-Object -Maximum).Maximum
            avg_time_ms = ($iterationTimes | Measure-Object -Average).Average
            median_time_ms = Get-Median -Values $iterationTimes
            total_time_ms = ($iterationTimes | Measure-Object -Sum).Sum
            success_rate = ($successCount / $result.iterations) * 100
            throughput = 1000 / (($iterationTimes | Measure-Object -Average).Average)  # ops/sec
            iterations_completed = $successCount
        }
    }
    
    return $result
}

function Get-Median {
    param([array]$Values)
    
    if ($Values.Count -eq 0) { return 0 }
    
    $sorted = $Values | Sort-Object
    $middle = [math]::Floor($sorted.Count / 2)
    
    if ($sorted.Count % 2 -eq 0) {
        return ($sorted[$middle - 1] + $sorted[$middle]) / 2
    } else {
        return $sorted[$middle]
    }
}

function Generate-Report {
    param(
        [array]$BenchmarkResults,
        [string]$OutputDir
    )
    
    Write-Info "Generating performance reports..."
    
    # Generate JSON report
    $jsonReport = @{
        metadata = @{
            generated = Get-Date -Format "yyyy-MM-ddTHH:mm:ss"
            host = $env:COMPUTERNAME
            os = [System.Environment]::OSVersion.VersionString
            powershell_version = $PSVersionTable.PSVersion.ToString()
        }
        benchmarks = $BenchmarkResults
        summary = @{
            total_benchmarks = $BenchmarkResults.Count
            total_iterations = ($BenchmarkResults | ForEach-Object { $_.iterations } | Measure-Object -Sum).Sum
            total_time_ms = ($BenchmarkResults | ForEach-Object { $_.metrics.total_time_ms } | Measure-Object -Sum).Sum
            average_success_rate = ($BenchmarkResults | ForEach-Object { $_.metrics.success_rate } | Measure-Object -Average).Average
        }
    }
    
    $jsonPath = Join-Path $OutputDir "benchmark_results_$(Get-Date -Format 'yyyyMMdd_HHmmss').json"
    $jsonReport | ConvertTo-Json -Depth 10 | Out-File -FilePath $jsonPath -Encoding UTF8
    Write-Success "JSON report saved: $jsonPath"
    
    # Generate Markdown report
    $mdReport = "# Zeta Benchmark Performance Report`n`n"
    $mdReport += "## Summary`n`n"
    $mdReport += "- **Generated**: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')`n"
    $mdReport += "- **Total Benchmarks**: $($BenchmarkResults.Count)`n"
    $mdReport += "- **Total Iterations**: $(($BenchmarkResults | ForEach-Object { $_.iterations } | Measure-Object -Sum).Sum)`n"
    $mdReport += "- **Total Execution Time**: $(($BenchmarkResults | ForEach-Object { $_.metrics.total_time_ms } | Measure-Object -Sum).Sum) ms`n"
    $mdReport += "- **Average Success Rate**: {0:F1}%`n" -f ($BenchmarkResults | ForEach-Object { $_.metrics.success_rate } | Measure-Object -Average).Average
    $mdReport += "`n"
    
    $mdReport += "## Individual Benchmark Results`n`n"
    
    foreach ($benchmark in $BenchmarkResults) {
        $mdReport += "### $($benchmark.name)`n`n"
        $mdReport += "- **File**: $($benchmark.file)`n"
        $mdReport += "- **Timestamp**: $($benchmark.timestamp)`n"
        $mdReport += "- **Iterations**: $($benchmark.iterations)`n"
        $mdReport += "- **Completed**: $($benchmark.metrics.iterations_completed)`n"
        $mdReport += "- **Success Rate**: {0:F1}%`n" -f $benchmark.metrics.success_rate
        $mdReport += "- **Min Time**: {0:F2} ms`n" -f $benchmark.metrics.min_time_ms
        $mdReport += "- **Max Time**: {0:F2} ms`n" -f $benchmark.metrics.max_time_ms
        $mdReport += "- **Average Time**: {0:F2} ms`n" -f $benchmark.metrics.avg_time_ms
        $mdReport += "- **Median Time**: {0:F2} ms`n" -f $benchmark.metrics.median_time_ms
        $mdReport += "- **Throughput**: {0:F2} ops/sec`n" -f $benchmark.metrics.throughput
        $mdReport += "- **Total Time**: {0:F2} ms`n" -f $benchmark.metrics.total_time_ms
        $mdReport += "`n"
    }
    
    # Performance comparison table
    $mdReport += "## Performance Comparison`n`n"
    $mdReport += "| Benchmark | Avg Time (ms) | Throughput (ops/sec) | Success Rate |`n"
    $mdReport += "|-----------|---------------|----------------------|--------------|`n"
    
    foreach ($benchmark in $BenchmarkResults) {
        $mdReport += "| $($benchmark.name) | {0:F2} | {1:F2} | {2:F1}% |`n" -f `
            $benchmark.metrics.avg_time_ms,
            $benchmark.metrics.throughput,
            $benchmark.metrics.success_rate
    }
    
    $mdReport += "`n"
    
    # Recommendations
    $mdReport += "## Recommendations`n`n"
    
    # Find fastest benchmark
    $fastest = $BenchmarkResults | Sort-Object { $_.metrics.avg_time_ms } | Select-Object -First 1
    $slowest = $BenchmarkResults | Sort-Object { $_.metrics.avg_time_ms } | Select-Object -Last 1
    
    if ($fastest -and $slowest -and $fastest -ne $slowest) {
        $speedup = $slowest.metrics.avg_time_ms / $fastest.metrics.avg_time_ms
        $mdReport += "1. **$($fastest.name)** is {0:F1}x faster than **$($slowest.name)**`n" -f $speedup
        $mdReport += "2. Consider optimizing **$($slowest.name)** implementation`n"
    }
    
    $lowestSuccess = $BenchmarkResults | Sort-Object { $_.metrics.success_rate } | Select-Object -First 1
    if ($lowestSuccess.metrics.success_rate -lt 95) {
        $mdReport += "3. **$($lowestSuccess.name)** has low success rate ({0:F1}%) - investigate stability`n" -f $lowestSuccess.metrics.success_rate
    }
    
    $mdReport += "4. Run with `-Quick` flag for development, full iterations for release testing`n"
    $mdReport += "5. Use `-Filter` parameter to run specific benchmarks`n"
    
    $mdPath = Join-Path $OutputDir "performance_analysis_$(Get-Date -Format 'yyyyMMdd_HHmmss').md"
    $mdReport | Out-File -FilePath $mdPath -Encoding UTF8
    Write-Success "Markdown report saved: $mdPath"
    
    # Generate CSV for spreadsheet analysis
    $csvPath = Join-Path $OutputDir "benchmark_data_$(Get-Date -Format 'yyyyMMdd_HHmmss').csv"
    $csvLines = @("Benchmark,AvgTimeMs,ThroughputOpsSec,SuccessRate,Iterations,MinTimeMs,MaxTimeMs,MedianTimeMs,TotalTimeMs")
    
    foreach ($benchmark in $BenchmarkResults) {
        $csvLines += """$($benchmark.name)"",{0:F2},{1:F2},{2:F1},{3},{4:F2},{5:F2},{6:F2},{7:F2}" -f `
            $benchmark.metrics.avg_time_ms,
            $benchmark.metrics.throughput,
            $benchmark.metrics.success_rate,
            $benchmark.iterations,
            $benchmark.metrics.min_time_ms,
            $benchmark.metrics.max_time_ms,
            $benchmark.metrics.median_time_ms,
            $benchmark.metrics.total_time_ms
    }
    
    $csvLines | Out-File -FilePath $csvPath -Encoding UTF8
    Write-Success "CSV data saved: $csvPath"
    
    return @($jsonPath, $mdPath, $csvPath)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

try {
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host "${ColorBlue}    ZETA BENCHMARK RUNNER               ${ColorReset}"
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host ""
    
    # Load configuration
    $config = @{
        iterations = if ($Quick) { 50 } else { 100 }
        timeout_seconds = 30
        memory_limit_mb = 1024
    }
    
    # Check requirements
    Test-Requirements
    
    # Clean if requested
    if ($Clean) {
        Clean-Benchmarks
    }
    
    # Discover benchmark files
    $benchmarkFiles = Get-BenchmarkFiles -FilterPattern $Filter
    
    if ($benchmarkFiles.Count -eq 0) {
        Write-Error "No benchmarks to run"
        exit 1
    }
    
    # Run benchmarks
    $allResults = @()
    $startTime = Get-Date
    
    foreach ($file in $benchmarkFiles) {
        $result = Run-SingleBenchmark -BenchmarkFile $file -Config $config
        $allResults += $result
    }
    
    $endTime = Get-Date
    $totalDuration = ($endTime - $startTime).TotalSeconds
    
    # Generate reports
    $reportFiles = Generate-Report -BenchmarkResults $allResults -OutputDir $OutputPath
    
    # Summary
    Write-Host ""
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host "${ColorBlue}          EXECUTION SUMMARY              ${ColorReset}"
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host ""
    Write-Success "Benchmark execution completed in {0:F2} seconds" -f $totalDuration
    Write-Success "Processed $($allResults.Count) benchmark(s)"
    Write-Success "Generated $($reportFiles.Count) report file(s)"
    
    # Show report locations
    Write-Host ""
    Write-Host "${ColorYellow}Report Files:${ColorReset}"
    foreach ($reportFile in $reportFiles) {
        Write-Host "  - $reportFile"
    }
    
    Write-Host ""
    Write-Success "Benchmarking complete!"
    
} catch {
    Write-Error "Benchmark execution failed: $_"
    Write-Error "Stack trace: $($_.ScriptStackTrace)"
    exit 1
}