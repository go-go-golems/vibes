#!/usr/bin/env python3

import os
import re
import json
import glob
import datetime
import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict, Counter
import argparse

def parse_log_line(line):
    """Parse a log line into components."""
    pattern = r'\[(.*?)\] (.*?) \[(.*?)\] \[(.*?)=(.*?)\] - (.*)'
    match = re.match(pattern, line)
    if match:
        timestamp_str, level, service, trace_id_key, trace_id, message = match.groups()
        try:
            timestamp = datetime.datetime.strptime(timestamp_str, '%Y-%m-%d %H:%M:%S.%f')
        except ValueError:
            timestamp = datetime.datetime.strptime(timestamp_str, '%Y-%m-%d %H:%M:%S')
        return {
            'timestamp': timestamp,
            'level': level,
            'service': service,
            'trace_id': trace_id,
            'message': message
        }
    
    # Try kafka/zookeeper pattern
    pattern = r'\[(.*?)\] (.*?) - (.*)'
    match = re.match(pattern, line)
    if match:
        timestamp_str, level, message = match.groups()
        try:
            timestamp = datetime.datetime.strptime(timestamp_str, '%Y-%m-%d %H:%M:%S.%f')
        except ValueError:
            timestamp = datetime.datetime.strptime(timestamp_str, '%Y-%m-%d %H:%M:%S')
        return {
            'timestamp': timestamp,
            'level': level,
            'service': 'kafka' if 'broker' in message or 'producer' in message or 'consumer' in message else 'zookeeper',
            'trace_id': '',
            'message': message
        }
    
    return None

def collect_logs(directory):
    """Collect and parse logs from the given directory."""
    logs = []
    
    # Find all log files
    log_files = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.log'):
                log_files.append(os.path.join(root, file))
    
    # Parse each log file
    for log_file in log_files:
        service = os.path.basename(log_file).replace('.log', '')
        with open(log_file, 'r') as f:
            lines = f.readlines()
            
            # Skip header lines
            start_idx = 0
            for i, line in enumerate(lines):
                if line.strip() == '':
                    start_idx = i + 1
                    break
            
            for line in lines[start_idx:]:
                parsed = parse_log_line(line.strip())
                if parsed:
                    parsed['file'] = log_file
                    logs.append(parsed)
    
    return logs

def analyze_logs(logs):
    """Analyze the logs and generate statistics."""
    analysis = {
        'total_logs': len(logs),
        'logs_per_service': defaultdict(int),
        'logs_per_level': defaultdict(int),
        'errors_per_service': defaultdict(int),
        'warnings_per_service': defaultdict(int),
        'common_errors': defaultdict(list),
        'traces': defaultdict(list),
        'service_latency': defaultdict(list),
        'api_response_times': [],
        'saga_transactions': []
    }
    
    # Count logs per service and level
    for log in logs:
        service = log['service']
        level = log['level']
        
        analysis['logs_per_service'][service] += 1
        analysis['logs_per_level'][level] += 1
        
        if level == 'ERROR':
            analysis['errors_per_service'][service] += 1
            analysis['common_errors'][service].append(log['message'])
        
        if level == 'WARN':
            analysis['warnings_per_service'][service] += 1
        
        # Group logs by trace ID
        if log['trace_id']:
            analysis['traces'][log['trace_id']].append(log)
    
    # Sort traces by timestamp of first log
    for trace_id, trace_logs in analysis['traces'].items():
        trace_logs.sort(key=lambda x: x['timestamp'])
        
        # Identify saga transactions
        if any('saga' in log['message'].lower() for log in trace_logs):
            analysis['saga_transactions'].append({
                'trace_id': trace_id,
                'start_time': trace_logs[0]['timestamp'],
                'end_time': trace_logs[-1]['timestamp'],
                'duration': (trace_logs[-1]['timestamp'] - trace_logs[0]['timestamp']).total_seconds(),
                'services_involved': list(set(log['service'] for log in trace_logs)),
                'success': not any(log['level'] == 'ERROR' for log in trace_logs),
                'logs': trace_logs
            })
        
        # Calculate service latency
        for i in range(1, len(trace_logs)):
            prev_log = trace_logs[i-1]
            curr_log = trace_logs[i]
            
            if prev_log['service'] != curr_log['service']:
                latency = (curr_log['timestamp'] - prev_log['timestamp']).total_seconds()
                analysis['service_latency'][f"{prev_log['service']} -> {curr_log['service']}"].append(latency)
    
    # Count frequency of common errors
    for service, errors in analysis['common_errors'].items():
        analysis['common_errors'][service] = Counter(errors).most_common(10)
    
    return analysis

def generate_charts(analysis, output_dir):
    """Generate charts from the log analysis."""
    os.makedirs(output_dir, exist_ok=True)
    
    # Chart 1: Logs per service
    plt.figure(figsize=(12, 6))
    services = list(analysis['logs_per_service'].keys())
    counts = list(analysis['logs_per_service'].values())
    plt.bar(services, counts)
    plt.title('Log Count by Service')
    plt.xlabel('Service')
    plt.ylabel('Log Count')
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'logs_per_service.png'))
    plt.close()
    
    # Chart 2: Logs per level
    plt.figure(figsize=(10, 6))
    levels = list(analysis['logs_per_level'].keys())
    counts = list(analysis['logs_per_level'].values())
    plt.bar(levels, counts, color=['green', 'blue', 'orange', 'red', 'purple'])
    plt.title('Log Count by Level')
    plt.xlabel('Log Level')
    plt.ylabel('Log Count')
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'logs_per_level.png'))
    plt.close()
    
    # Chart 3: Errors and warnings per service
    plt.figure(figsize=(12, 6))
    services = list(set(list(analysis['errors_per_service'].keys()) + list(analysis['warnings_per_service'].keys())))
    errors = [analysis['errors_per_service'].get(service, 0) for service in services]
    warnings = [analysis['warnings_per_service'].get(service, 0) for service in services]
    
    x = np.arange(len(services))
    width = 0.35
    
    fig, ax = plt.subplots(figsize=(12, 6))
    rects1 = ax.bar(x - width/2, errors, width, label='Errors', color='red')
    rects2 = ax.bar(x + width/2, warnings, width, label='Warnings', color='orange')
    
    ax.set_title('Errors and Warnings by Service')
    ax.set_xlabel('Service')
    ax.set_ylabel('Count')
    ax.set_xticks(x)
    ax.set_xticklabels(services, rotation=45, ha='right')
    ax.legend()
    
    fig.tight_layout()
    plt.savefig(os.path.join(output_dir, 'errors_warnings_per_service.png'))
    plt.close()
    
    # Chart 4: Saga transaction success rate
    if analysis['saga_transactions']:
        success_count = sum(1 for saga in analysis['saga_transactions'] if saga['success'])
        failure_count = len(analysis['saga_transactions']) - success_count
        
        plt.figure(figsize=(8, 8))
        plt.pie([success_count, failure_count], labels=['Success', 'Failure'], 
                autopct='%1.1f%%', colors=['green', 'red'], startangle=90)
        plt.title('Saga Transaction Success Rate')
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'saga_success_rate.png'))
        plt.close()
        
        # Chart 5: Saga transaction durations
        durations = [saga['duration'] for saga in analysis['saga_transactions']]
        
        plt.figure(figsize=(10, 6))
        plt.hist(durations, bins=10, alpha=0.75)
        plt.title('Saga Transaction Durations')
        plt.xlabel('Duration (seconds)')
        plt.ylabel('Count')
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'saga_durations.png'))
        plt.close()
    
    # Chart 6: Service latency
    if analysis['service_latency']:
        plt.figure(figsize=(12, 8))
        
        service_pairs = list(analysis['service_latency'].keys())
        avg_latencies = [np.mean(latencies) for latencies in analysis['service_latency'].values()]
        
        # Sort by average latency
        sorted_indices = np.argsort(avg_latencies)[::-1]
        service_pairs = [service_pairs[i] for i in sorted_indices]
        avg_latencies = [avg_latencies[i] for i in sorted_indices]
        
        plt.barh(service_pairs, avg_latencies)
        plt.title('Average Service Communication Latency')
        plt.xlabel('Latency (seconds)')
        plt.ylabel('Service Pair')
        plt.tight_layout()
        plt.savefig(os.path.join(output_dir, 'service_latency.png'))
        plt.close()

def generate_report(analysis, charts_dir, output_file):
    """Generate a markdown report from the log analysis."""
    with open(output_file, 'w') as f:
        f.write("# Log Analysis Report\n\n")
        
        # General Statistics
        f.write("## 1. General Statistics\n\n")
        f.write(f"Total logs analyzed: {analysis['total_logs']}\n\n")
        
        f.write("### 1.1 Logs per Service\n\n")
        f.write("| Service | Log Count | Errors | Warnings |\n")
        f.write("|---------|-----------|--------|----------|\n")
        for service in sorted(analysis['logs_per_service'].keys()):
            log_count = analysis['logs_per_service'][service]
            error_count = analysis['errors_per_service'].get(service, 0)
            warning_count = analysis['warnings_per_service'].get(service, 0)
            f.write(f"| {service} | {log_count} | {error_count} | {warning_count} |\n")
        
        f.write("\n![Logs per Service](charts/logs_per_service.png)\n\n")
        
        f.write("### 1.2 Logs per Level\n\n")
        f.write("| Log Level | Count |\n")
        f.write("|-----------|-------|\n")
        for level in sorted(analysis['logs_per_level'].keys()):
            count = analysis['logs_per_level'][level]
            f.write(f"| {level} | {count} |\n")
        
        f.write("\n![Logs per Level](charts/logs_per_level.png)\n\n")
        f.write("\n![Errors and Warnings per Service](charts/errors_warnings_per_service.png)\n\n")
        
        # Error Analysis
        f.write("## 2. Error Analysis\n\n")
        for service, errors in sorted(analysis['common_errors'].items()):
            f.write(f"### 2.{list(sorted(analysis['common_errors'].keys())).index(service) + 1} Common Errors in {service}\n\n")
            f.write("| Error Message | Count |\n")
            f.write("|---------------|-------|\n")
            for error, count in errors:
                # Truncate long error messages
                error_msg = error[:100] + "..." if len(error) > 100 else error
                f.write(f"| {error_msg} | {count} |\n")
            f.write("\n")
        
        # Saga Transaction Analysis
        if analysis['saga_transactions']:
            f.write("## 3. Saga Transaction Analysis\n\n")
            f.write(f"Total saga transactions: {len(analysis['saga_transactions'])}\n\n")
            
            success_count = sum(1 for saga in analysis['saga_transactions'] if saga['success'])
            f.write(f"Success rate: {success_count / len(analysis['saga_transactions']) * 100:.1f}%\n\n")
            
            f.write("![Saga Success Rate](charts/saga_success_rate.png)\n\n")
            
            f.write("### 3.1 Saga Transaction Durations\n\n")
            durations = [saga['duration'] for saga in analysis['saga_transactions']]
            f.write(f"Average duration: {np.mean(durations):.2f} seconds\n")
            f.write(f"Minimum duration: {min(durations):.2f} seconds\n")
            f.write(f"Maximum duration: {max(durations):.2f} seconds\n\n")
            
            f.write("![Saga Durations](charts/saga_durations.png)\n\n")
            
            f.write("### 3.2 Sample Successful Saga\n\n")
            successful_sagas = [saga for saga in analysis['saga_transactions'] if saga['success']]
            if successful_sagas:
                sample_saga = sorted(successful_sagas, key=lambda x: len(x['logs']), reverse=True)[0]
                f.write(f"Trace ID: {sample_saga['trace_id']}\n")
                f.write(f"Duration: {sample_saga['duration']:.2f} seconds\n")
                f.write(f"Services involved: {', '.join(sample_saga['services_involved'])}\n\n")
                
                f.write("| Timestamp | Service | Level | Message |\n")
                f.write("|-----------|---------|-------|--------|\n")
                for log in sample_saga['logs']:
                    f.write(f"| {log['timestamp'].strftime('%H:%M:%S.%f')[:-3]} | {log['service']} | {log['level']} | {log['message'][:80]}... |\n")
            
            f.write("\n### 3.3 Sample Failed Saga\n\n")
            failed_sagas = [saga for saga in analysis['saga_transactions'] if not saga['success']]
            if failed_sagas:
                sample_saga = sorted(failed_sagas, key=lambda x: len(x['logs']), reverse=True)[0]
                f.write(f"Trace ID: {sample_saga['trace_id']}\n")
                f.write(f"Duration: {sample_saga['duration']:.2f} seconds\n")
                f.write(f"Services involved: {', '.join(sample_saga['services_involved'])}\n\n")
                
                f.write("| Timestamp | Service | Level | Message |\n")
                f.write("|-----------|---------|-------|--------|\n")
                for log in sample_saga['logs']:
                    f.write(f"| {log['timestamp'].strftime('%H:%M:%S.%f')[:-3]} | {log['service']} | {log['level']} | {log['message'][:80]}... |\n")
        
        # Service Communication Analysis
        if analysis['service_latency']:
            f.write("\n## 4. Service Communication Analysis\n\n")
            f.write("### 4.1 Average Service Latency\n\n")
            f.write("| Service Pair | Average Latency (seconds) |\n")
            f.write("|-------------|---------------------------|\n")
            
            # Sort by average latency
            service_pairs = list(analysis['service_latency'].keys())
            avg_latencies = [np.mean(latencies) for latencies in analysis['service_latency'].values()]
            
            # Sort by average latency
            sorted_indices = np.argsort(avg_latencies)[::-1]
            service_pairs = [service_pairs[i] for i in sorted_indices]
            avg_latencies = [avg_latencies[i] for i in sorted_indices]
            
            for pair, latency in zip(service_pairs, avg_latencies):
                f.write(f"| {pair} | {latency:.3f} |\n")
            
            f.write("\n![Service Latency](charts/service_latency.png)\n\n")
        
        # Conclusions
        f.write("## 5. Conclusions and Recommendations\n\n")
        
        # Error patterns
        high_error_services = [service for service, count in analysis['errors_per_service'].items() 
                              if count > analysis['logs_per_service'][service] * 0.1]
        if high_error_services:
            f.write("### 5.1 Services with High Error Rates\n\n")
            f.write("The following services have error rates above 10% and require attention:\n\n")
            for service in high_error_services:
                error_rate = analysis['errors_per_service'][service] / analysis['logs_per_service'][service] * 100
                f.write(f"- **{service}**: {error_rate:.1f}% error rate\n")
            f.write("\n")
        
        # Saga performance
        if analysis['saga_transactions']:
            f.write("### 5.2 Saga Transaction Performance\n\n")
            success_rate = success_count / len(analysis['saga_transactions']) * 100
            if success_rate < 95:
                f.write(f"Saga transaction success rate is only {success_rate:.1f}%, which is below the recommended 95% threshold. ")
                f.write("Consider reviewing compensation mechanisms and error handling in the saga orchestration.\n\n")
            else:
                f.write(f"Saga transaction success rate is {success_rate:.1f}%, which is good. ")
                f.write("Continue monitoring for any changes in the success rate.\n\n")
            
            # Duration analysis
            avg_duration = np.mean(durations)
            if avg_duration > 5.0:
                f.write(f"Average saga transaction duration is {avg_duration:.2f} seconds, which may be improved. ")
                f.write("Consider optimizing service communication or database operations to reduce latency.\n\n")
            else:
                f.write(f"Average saga transaction duration is {avg_duration:.2f} seconds, which is acceptable. ")
                f.write("Continue monitoring for any increases in transaction duration.\n\n")
        
        # Service latency
        if analysis['service_latency']:
            slow_service_pairs = [pair for pair, latencies in analysis['service_latency'].items() 
                                if np.mean(latencies) > 0.5]
            if slow_service_pairs:
                f.write("### 5.3 High Latency Service Communications\n\n")
                f.write("The following service communications have high latency and could be optimized:\n\n")
                for pair in slow_service_pairs:
                    avg_latency = np.mean(analysis['service_latency'][pair])
                    f.write(f"- **{pair}**: {avg_latency:.3f} seconds average latency\n")
                f.write("\n")
        
        # General recommendations
        f.write("### 5.4 General Recommendations\n\n")
        f.write("1. **Implement Circuit Breakers**: For services with frequent connection errors to external systems\n")
        f.write("2. **Enhance Error Logging**: Add more context to error messages for better troubleshooting\n")
        f.write("3. **Optimize Database Connections**: Consider connection pooling or caching frequently accessed data\n")
        f.write("4. **Implement Retry Mechanisms**: For transient failures in service communication\n")
        f.write("5. **Add Health Checks**: Monitor service health and implement automatic recovery\n")
        
def main():
    parser = argparse.ArgumentParser(description='Analyze microservice logs')
    parser.add_argument('--logs_dir', default='logs', help='Directory containing log files')
    parser.add_argument('--output_dir', default='log_analysis', help='Directory to store the analysis output')
    args = parser.parse_args()
    
    # Ensure output directory exists
    os.makedirs(args.output_dir, exist_ok=True)
    
    # Collect and analyze logs
    logs = collect_logs(args.logs_dir)
    analysis = analyze_logs(logs)
    
    # Generate charts and report
    charts_dir = os.path.join(args.output_dir, 'charts')
    generate_charts(analysis, charts_dir)
    
    report_file = os.path.join(args.output_dir, 'log_analysis_report.md')
    generate_report(analysis, 'charts', report_file)
    
    # Save analysis as JSON for further processing
    with open(os.path.join(args.output_dir, 'analysis.json'), 'w') as f:
        # Convert datetime objects to strings
        analysis_serializable = {}
        for key, value in analysis.items():
            if key == 'traces':
                # Skip traces as they contain non-serializable objects
                continue
            elif key == 'saga_transactions':
                # Convert datetime objects in saga transactions
                serializable_sagas = []
                for saga in value:
                    serializable_saga = {
                        'trace_id': saga['trace_id'],
                        'start_time': saga['start_time'].isoformat(),
                        'end_time': saga['end_time'].isoformat(),
                        'duration': saga['duration'],
                        'services_involved': saga['services_involved'],
                        'success': saga['success']
                    }
                    serializable_sagas.append(serializable_saga)
                analysis_serializable[key] = serializable_sagas
            else:
                analysis_serializable[key] = value
        
        json.dump(analysis_serializable, f, indent=2, default=str)
    
    print(f"Analysis complete. Report saved to {report_file}")

if __name__ == "__main__":
    main()