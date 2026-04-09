#!/usr/bin/env pwsh
# REAL Benchmark Runner for Zeta
# Actually compiles and runs Zeta code to get real performance numbers

param(
    [string]$BenchmarkFile = "benchmarks/murphy_sieve_performance_benchmark.z",
    [int]$Iterations = 10,
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

# Colors for output
$ColorReset = "$([char]27)[0m"
$ColorGreen = "$([char]27)[32m"
$ColorYellow = "$([char]27)[33m"
$ColorRed = "$([char]27)[31m"
$ColorBlue = "$([char]27)[34m"
$ColorCyan = "$([char]27)[36m"

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

function Measure-Zeta-Execution {
    param(
        [string]$ZetaFile,
        [int]$Iteration
    )
    
    Write-Info "Iteration ${Iteration}: Compiling and running $ZetaFile"
    
    # Start timing
    $stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
    
    # Compile and run the Zeta program
    try {
        $output = cargo run --bin zetac -- $ZetaFile 2>&1
        $stopwatch.Stop()
        
        # Parse the output to get the result
        $resultLine = $output | Select-String -Pattern "^Result: (\d+)$" | Select-Object -Last 1
        
        if ($resultLine) {
            $result = [int]$resultLine.Matches.Groups[1].Value
            $executionTime = $stopwatch.Elapsed.TotalMilliseconds
            
            if ($Verbose) {
                Write-Host "  Result: $result, Time: ${executionTime}ms"
            }
            
            return @{
                Success = $true
                TimeMs = $executionTime
                Result = $result
                Output = $output
            }
        } else {
            Write-Warning "  No result found in output"
            if ($Verbose) {
                Write-Host "  Output: $output"
            }
            
            return @{
                Success = $false
                TimeMs = $stopwatch.Elapsed.TotalMilliseconds
                Error = "No result found"
                Output = $output
            }
        }
    } catch {
        $stopwatch.Stop()
        Write-Error "  Failed to compile/run: $_"
        
        return @{
            Success = $false
            TimeMs = $stopwatch.Elapsed.TotalMilliseconds
            Error = $_.Exception.Message
            Output = $_
        }
    }
}

function Run-Benchmark {
    param(
        [string]$BenchmarkFile,
        [int]$Iterations
    )
    
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host "${ColorBlue}    REAL ZETA BENCHMARK RUNNER          ${ColorReset}"
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host ""
    
    # Check if benchmark file exists
    if (-not (Test-Path $BenchmarkFile)) {
        Write-Error "Benchmark file not found: $BenchmarkFile"
        return $null
    }
    
    Write-Info "Benchmark file: $BenchmarkFile"
    Write-Info "Iterations: $Iterations"
    Write-Info ""
    
    # Run iterations
    $results = @()
    $successfulRuns = 0
    
    for ($i = 1; $i -le $Iterations; $i++) {
        $result = Measure-Zeta-Execution -ZetaFile $BenchmarkFile -Iteration $i
        $results += $result
        
        if ($result.Success) {
            $successfulRuns++
        }
        
        # Small delay between runs
        if ($i -lt $Iterations) {
            Start-Sleep -Milliseconds 100
        }
    }
    
    # Calculate statistics
    $successfulTimes = $results | Where-Object { $_.Success } | ForEach-Object { $_.TimeMs }
    
    if ($successfulTimes.Count -gt 0) {
        $stats = @{
            MinTime = ($successfulTimes | Measure-Object -Minimum).Minimum
            MaxTime = ($successfulTimes | Measure-Object -Maximum).Maximum
            AvgTime = ($successfulTimes | Measure-Object -Average).Average
            TotalTime = ($successfulTimes | Measure-Object -Sum).Sum
            SuccessRate = ($successfulRuns / $Iterations) * 100
            Iterations = $Iterations
            SuccessfulIterations = $successfulRuns
        }
    } else {
        $stats = @{
            MinTime = 0
            MaxTime = 0
            AvgTime = 0
            TotalTime = 0
            SuccessRate = 0
            Iterations = $Iterations
            SuccessfulIterations = 0
        }
    }
    
    return @{
        File = $BenchmarkFile
        Results = $results
        Statistics = $stats
    }
}

function Generate-Report {
    param(
        [hashtable]$BenchmarkData
    )
    
    $stats = $BenchmarkData.Statistics
    
    Write-Host ""
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host "${ColorBlue}          BENCHMARK RESULTS              ${ColorReset}"
    Write-Host "${ColorBlue}=========================================${ColorReset}"
    Write-Host ""
    
    Write-Info "Benchmark: $($BenchmarkData.File)"
    Write-Info "Iterations: $($stats.Iterations)"
    Write-Info "Successful: $($stats.SuccessfulIterations)"
    Write-Info "Success Rate: {0:F1}%" -f $stats.SuccessRate
    Write-Host ""
    
    if ($stats.SuccessfulIterations -gt 0) {
        Write-Success "Minimum Time: {0:F2} ms" -f $stats.MinTime
        Write-Success "Maximum Time: {0:F2} ms" -f $stats.MaxTime
        Write-Success "Average Time: {0:F2} ms" -f $stats.AvgTime
        Write-Success "Total Time: {0:F2} ms" -f $stats.TotalTime
        Write-Success "Throughput: {0:F2} runs/sec" -f (1000 / $stats.AvgTime)
    } else {
        Write-Error "No successful runs - cannot calculate performance metrics"
    }
    
    # Show individual results if verbose
    if ($Verbose -and $BenchmarkData.Results.Count -gt 0) {
        Write-Host ""
        Write-Info "Individual Run Results:"
        for ($i = 0; $i -lt $BenchmarkData.Results.Count; $i++) {
            $result = $BenchmarkData.Results[$i]
            $status = if ($result.Success) { "OK" } else { "FAIL" }
            $color = if ($result.Success) { $ColorGreen } else { $ColorRed }
            
            Write-Host "  Run $($i+1): $color$status$ColorReset Time: {0:F2}ms" -f $result.TimeMs
            if (-not $result.Success -and $result.Error) {
                Write-Host "    Error: $($result.Error)"
            }
        }
    }
    
    # Generate JSON report
    $report = @{
        timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ss"
        benchmark = $BenchmarkData.File
        statistics = $stats
        runs = $BenchmarkData.Results
    }
    
    $reportPath = "real_benchmark_results_$(Get-Date -Format 'yyyyMMdd_HHmmss').json"
    $report | ConvertTo-Json -Depth 10 | Out-File -FilePath $reportPath -Encoding UTF8
    
    Write-Host ""
    Write-Success "Report saved to: $reportPath"
    
    return $reportPath
}

# Main execution
try {
    $benchmarkData = Run-Benchmark -BenchmarkFile $BenchmarkFile -Iterations $Iterations
    
    if ($benchmarkData) {
        $reportPath = Generate-Report -BenchmarkData $benchmarkData
        
        Write-Host ""
        Write-Success "REAL benchmark execution complete!"
        Write-Host ""
        Write-Info "Next steps:"
        Write-Info "1. Run with different benchmark files"
        Write-Info "2. Increase iterations for more accurate results"
        Write-Info "3. Compare with simulated benchmark results"
    }
} catch {
    Write-Error "Benchmark execution failed: $_"
    exit 1
}