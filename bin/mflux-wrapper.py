#!/usr/bin/env -S uv run -p 3.12 --script 
# /// script
# dependencies = [
#   "mflux",
# ]
# ///
"""
MFlux Generator Wrapper

A tool for managing and exploring prompts with mflux-generate, including:
- Running variations with random seeds
- Storing metadata
- Generating HTML pages for output review
- Tracking runs for iterative development
"""

import argparse
import json
import os
import subprocess
import datetime
import random
import shutil
from pathlib import Path

STYLE_FILE = os.path.expanduser("~/.mflux_styles.json")

def load_styles():
    """Load saved styles from the style file."""
    if os.path.exists(STYLE_FILE):
        try:
            with open(STYLE_FILE, 'r') as f:
                return json.load(f)
        except json.JSONDecodeError:
            return {}
    return {}

def save_style(name, description):
    """Save a new style to the style file."""
    styles = load_styles()
    styles[name] = description
    os.makedirs(os.path.dirname(STYLE_FILE), exist_ok=True)
    with open(STYLE_FILE, 'w') as f:
        json.dump(styles, f, indent=2)

def list_styles():
    """List all saved styles."""
    styles = load_styles()
    if not styles:
        print("No saved styles found.")
        return
    
    print("\nSaved Styles:")
    print("-" * 40)
    for name, description in styles.items():
        print(f"{name}:")
        print(f"  {description}")
        print("-" * 40)

def get_style(name):
    """Get a style by name."""
    styles = load_styles()
    return styles.get(name)

def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Wrapper for mflux-generate to manage prompt exploration")
    
    # Style management commands
    style_group = parser.add_mutually_exclusive_group()
    style_group.add_argument("--save-style", nargs=2, metavar=('NAME', 'DESCRIPTION'),
                        help="Save a new style with name and description")
    style_group.add_argument("--list-styles", action="store_true",
                        help="List all saved styles")
    style_group.add_argument("--style", type=str, metavar='STYLE_NAME',
                        help="Apply a saved style to the prompt")
    
    # Brainstorm commands
    brainstorm_group = parser.add_mutually_exclusive_group()
    brainstorm_group.add_argument("--brainstorm", type=str, metavar='CONCEPT',
                        help="Generate 5 prompt ideas for the given concept using Claude")
    brainstorm_group.add_argument("--run-prompt", type=int, metavar='INDEX',
                        help="Run a single prompt from the last brainstorm by index number")
    
    # Arguments
    parser.add_argument("--prompt", type=str, help="The prompt to use for generation (if not specified, uses last prompt from current session)")
    parser.add_argument("--model", type=str, default="schnell", choices=["schnell", "dev"], 
                        help="Model type to use (schnell or dev, default: schnell)")
    parser.add_argument("--new", action="store_true", help="Force creation of new output directory")
    parser.add_argument("--iterations", type=int, default=4, help="Number of iterations to generate (default: 4)")
    parser.add_argument("--vary-seed", action="store_true", help="Generate variations using different random seeds (cannot be used with --seed)")
    parser.add_argument("--vary-steps", type=str, help="Comma-separated list of step counts to iterate through (requires --seed)")
    parser.add_argument("--no-watch", action="store_true", help="Don't start live-server to monitor changes")
    
    # Resolution options (mutually exclusive)
    resolution_group = parser.add_mutually_exclusive_group()
    resolution_group.add_argument("--resolution", type=str, default="1024x1024", 
                        help="Resolution in format WIDTHxHEIGHT (default: 1024x1024)")
    resolution_group.add_argument("--landscape", action="store_true", help="Use 16:9 landscape format (1024x576)")
    resolution_group.add_argument("--landscape-sm", action="store_true", help="Use 16:9 landscape format small (512x288)")
    resolution_group.add_argument("--landscape-lg", action="store_true", help="Use 16:9 landscape format large (1536x864)")
    resolution_group.add_argument("--landscape-xl", action="store_true", help="Use 16:9 landscape format xl (2048x1152)")
    resolution_group.add_argument("--portrait", action="store_true", help="Use 3:4 portrait format (768x1024)")
    resolution_group.add_argument("--portrait-sm", action="store_true", help="Use 3:4 portrait format small (384x512)")
    resolution_group.add_argument("--portrait-lg", action="store_true", help="Use 3:4 portrait format large (1152x1536)")
    resolution_group.add_argument("--portrait-xl", action="store_true", help="Use 3:4 portrait format xl (1536x2048)")
    resolution_group.add_argument("--square-sm", action="store_true", help="Use square format small (512x512)")
    resolution_group.add_argument("--square-xl", action="store_true", help="Use square format xl (2048x2048)")
    
    # Other optional arguments
    parser.add_argument("--steps", type=int, help="Number of steps (defaults: 1 for schnell, 5 for dev)")
    parser.add_argument("--metadata", action="store_true", help="Include metadata in output")
    parser.add_argument("--output-dir", type=str, help="Output directory (default: auto-generated based on date)")
    parser.add_argument("--seed", type=int, help="Starting seed (random if not provided, cannot be used with --vary-seed)")
    
    args = parser.parse_args()
    
    # Handle style commands
    if args.save_style:
        save_style(args.save_style[0], args.save_style[1])
        print(f"Style '{args.save_style[0]}' saved successfully.")
        exit(0)
    elif args.list_styles:
        list_styles()
        exit(0)
    elif args.style:
        if args.style.lower() == "none":
            # Clear any existing style
            args.style = None
            args.style_desc = None
        else:
            style_desc = get_style(args.style)
            if not style_desc:
                raise ValueError(f"Style '{args.style}' not found. Use --list-styles to see available styles.")
            # Store the style description for later use
            args.style_desc = style_desc
    
    # Set resolution based on format flags
    if args.landscape:
        args.resolution = "1024x576"
    elif args.landscape_sm:
        args.resolution = "512x288"
    elif args.landscape_lg:
        args.resolution = "1536x864"
    elif args.landscape_xl:
        args.resolution = "2048x1152"
    elif args.portrait:
        args.resolution = "768x1024"
    elif args.portrait_sm:
        args.resolution = "384x512"
    elif args.portrait_lg:
        args.resolution = "1152x1536"
    elif args.portrait_xl:
        args.resolution = "1536x2048"
    elif args.square_sm:
        args.resolution = "512x512"
    elif args.square_xl:
        args.resolution = "2048x2048"
    
    # Validate vary-steps command
    if args.vary_steps:
        if args.seed is None:
            raise ValueError("--vary-steps requires --seed to be specified")
        try:
            # Handle both single value and comma-separated list
            if ',' in args.vary_steps:
                args.vary_steps_list = [int(s.strip()) for s in args.vary_steps.split(",")]
            else:
                args.vary_steps_list = [int(args.vary_steps.strip())]
        except ValueError:
            raise ValueError("--vary-steps must be a comma-separated list of integers or a single integer (e.g., '1,3,5,9' or '3')")
        args.iterations = len(args.vary_steps_list)  # Override iterations to match vary-steps list
        args.steps = None  # Clear any manually set steps
    
    # Validate vary-seed and seed combination
    if args.vary_seed and args.seed is not None:
        raise ValueError("--vary-seed cannot be used with --seed")
        
    return args

def get_default_settings():
    """Get default settings for a new run."""
    return {
        "prompt": None,
        "model": "schnell",
        "steps": 1,
        "resolution": "1024x1024",
        "iterations": 4,
        "metadata": False,
        "vary_seed": False,
        "seed": None,
        "style": None,
        "vary_steps": None
    }

def load_last_settings(base_dir):
    """Load settings from the most recent run in the directory."""
    runs = sorted([d for d in os.listdir(base_dir) if d.startswith("run_")], 
                 key=lambda x: int(x.split("_")[1]), reverse=True)
    
    for run in runs:
        info_path = os.path.join(base_dir, run, "run_info.json")
        if os.path.exists(info_path):
            try:
                with open(info_path, "r") as f:
                    info = json.load(f)
                    # Extract command args from the last run
                    return info.get("command_args", get_default_settings())
            except:
                continue
    return get_default_settings()

def save_run_info(run_dir, settings, results):
    """Save run information for future reference."""
    info = {
        "timestamp": datetime.datetime.now().isoformat(),
        "command_args": settings,  # Store all settings in command_args
        "results": [{
            "index": result["index"],
            "seed": result["seed"],
            "steps": result["steps"],
            "file_path": result["file_path"],
            "metadata_path": result.get("metadata_path"),
            "generation_time": result.get("generation_time", 0)
        } for result in results]
    }
    
    with open(os.path.join(run_dir, "run_info.json"), "w") as f:
        json.dump(info, f, indent=2)

class RunInfo:
    """Class to handle all run info operations."""
    def __init__(self, run_dir):
        self.run_dir = run_dir
        self.info_path = os.path.join(run_dir, "run_info.json")
        self.info = self._load_info()
    
    def _load_info(self):
        """Load run info from file or create new if doesn't exist."""
        if os.path.exists(self.info_path):
            try:
                with open(self.info_path, "r") as f:
                    return json.load(f)
            except Exception as e:
                print(f"Warning: Could not load run info: {e}")
                return self._create_empty_info()
        return self._create_empty_info()
    
    def _create_empty_info(self):
        """Create empty run info structure."""
        return {
            "timestamp": datetime.datetime.now().isoformat(),
            "command_args": get_default_settings(),
            "results": []
        }
    
    def save(self):
        """Save current run info to file."""
        with open(self.info_path, "w") as f:
            json.dump(self.info, f, indent=2)
    
    def update_settings(self, settings):
        """Update command args in run info."""
        self.info["command_args"] = settings
        self.save()
    
    def add_result(self, result):
        """Add a new result to the run info."""
        self.info["results"].append(result)
        self.save()
    
    def get_settings(self):
        """Get current settings from run info."""
        return self.info.get("command_args", get_default_settings())
    
    def get_results(self):
        """Get current results from run info."""
        return self.info.get("results", [])
    
    def get_current_images(self):
        """Get number of current images generated."""
        return len(self.get_results())
    
    def get_total_iterations(self):
        """Get total number of iterations from settings."""
        return self.get_settings().get("iterations", 0)
    
    def is_complete(self):
        """Check if run is complete."""
        return self.get_current_images() == self.get_total_iterations()

def create_output_directories(args):
    """Create the output directory structure."""
    # Create base directory if not specified
    if not args.output_dir:
        # Check if there's a recent session directory (within last hour) and --new wasn't specified
        now = datetime.datetime.now()
        recent_dirs = []
        
        if not args.new:  # Only look for recent dirs if --new wasn't specified
            for d in os.listdir('.'):
                if d.startswith('mflux_output_'):
                    try:
                        dir_time = datetime.datetime.strptime(d.split('_', 2)[2], "%Y%m%d_%H%M%S")
                        if (now - dir_time).total_seconds() < 3600:  # Within last hour
                            recent_dirs.append((dir_time, d))
                    except ValueError:
                        continue
        
        if recent_dirs:
            # Use most recent session directory
            base_dir = sorted(recent_dirs, reverse=True)[0][1]
            
            # If no settings specified, try to get the last ones used
            last_settings = get_last_settings(base_dir)
            if last_settings:
                # Apply last prompt if none specified
                if args.prompt is None:
                    args.prompt = last_settings["prompt"]
                    # If we have a style from args, apply it to the last prompt
                    if hasattr(args, 'style_desc'):
                        args.prompt = f"{args.prompt}, {args.style_desc}"
                    # Otherwise, if we had a style from last time and no new style specified, reuse it
                    elif last_settings["style"] and not args.style:
                        style_desc = get_style(last_settings["style"])
                        if style_desc:
                            args.prompt = f"{args.prompt}, {style_desc}"
                
                # Apply last resolution if none specified (no format flag used)
                if not any([args.landscape, args.landscape_sm, args.landscape_lg, args.landscape_xl, args.portrait, args.portrait_sm, args.portrait_lg, args.portrait_xl, args.square_sm, args.square_xl]) and args.resolution == "1024x1024":
                    args.resolution = last_settings["resolution"]
                
                # Apply last iterations if none specified and not using vary-steps
                if args.iterations == 4 and not hasattr(args, 'vary_steps_list'):
                    args.iterations = last_settings["iterations"]
                
                # Apply last model if not specified
                if args.model == "schnell":  # Only apply if using default
                    args.model = last_settings["model"]
                
                # Apply last steps if not specified
                if args.steps is None and not hasattr(args, 'vary_steps_list'):
                    args.steps = last_settings["steps"]
                
                # Apply last metadata setting if not specified
                if not args.metadata:
                    args.metadata = last_settings["metadata"]
                
                # Apply last vary_seed setting if not specified
                if not args.vary_seed:
                    args.vary_seed = last_settings["vary_seed"]
                
                # Apply last seed if not specified and not using vary_seed
                if args.seed is None and not args.vary_seed:
                    args.seed = last_settings["seed"]
                
                if args.prompt is None:
                    raise ValueError("No prompt specified and couldn't find last prompt in current session")
        else:
            # Create new session directory
            timestamp = now.strftime("%Y%m%d_%H%M%S")
            base_dir = f"mflux_output_{timestamp}"
            if args.prompt is None:
                raise ValueError("No prompt specified and no recent session to get prompt from")
    else:
        base_dir = args.output_dir
        if args.prompt is None:
            last_settings = get_last_settings(base_dir)
            if last_settings:
                args.prompt = last_settings["prompt"]
                # If we have a style from args, apply it to the last prompt
                if hasattr(args, 'style_desc'):
                    args.prompt = f"{args.prompt}, {args.style_desc}"
                # Otherwise, if we had a style from last time and no new style specified, reuse it
                elif last_settings["style"] and not args.style:
                    style_desc = get_style(last_settings["style"])
                    if style_desc:
                        args.prompt = f"{args.prompt}, {style_desc}"
                
                # Apply last resolution if none specified (no format flag used)
                if not any([args.landscape, args.landscape_sm, args.landscape_lg, args.landscape_xl, args.portrait, args.portrait_sm, args.portrait_lg, args.portrait_xl, args.square_sm, args.square_xl]) and args.resolution == "1024x1024":
                    args.resolution = last_settings["resolution"]
                
                # Apply last iterations if none specified and not using vary-steps
                if args.iterations == 4 and not hasattr(args, 'vary_steps_list'):
                    args.iterations = last_settings["iterations"]
                
                # Apply last model if not specified
                if args.model == "schnell":  # Only apply if using default
                    args.model = last_settings["model"]
                
                # Apply last steps if not specified
                if args.steps is None and not hasattr(args, 'vary_steps_list'):
                    args.steps = last_settings["steps"]
                
                # Apply last metadata setting if not specified
                if not args.metadata:
                    args.metadata = last_settings["metadata"]
                
                # Apply last vary_seed setting if not specified
                if not args.vary_seed:
                    args.vary_seed = last_settings["vary_seed"]
                
                # Apply last seed if not specified and not using vary_seed
                if args.seed is None and not args.vary_seed:
                    args.seed = last_settings["seed"]
            else:
                raise ValueError("No prompt specified and couldn't find last prompt in output directory")
    
    # Ensure base directory exists
    os.makedirs(base_dir, exist_ok=True)
    
    # Determine the next run number
    existing_runs = [d for d in os.listdir(base_dir) if d.startswith("run_")]
    if existing_runs:
        run_numbers = [int(run.split("_")[1]) for run in existing_runs]
        next_run = max(run_numbers) + 1
    else:
        next_run = 1
    
    run_dir = os.path.join(base_dir, f"run_{next_run}")
    os.makedirs(run_dir, exist_ok=True)
    
    # Create initial run_info.json with empty results
    initial_info = {
        "timestamp": datetime.datetime.now().isoformat(),
        "command_args": {
            "prompt": args.prompt,
            "model": args.model,
            "resolution": args.resolution,
            "metadata": args.metadata,
            "style": args.style if hasattr(args, 'style') else None,
            "steps": args.vary_steps if hasattr(args, 'vary_steps_list') else (args.steps if args.steps is not None else get_default_steps(args.model)),
            "vary_steps": args.vary_steps if hasattr(args, 'vary_steps') else None,
            "vary_seed": args.vary_seed,
            "seed": args.seed,
            "iterations": args.iterations
        },
        "results": []
    }
    
    with open(os.path.join(run_dir, "run_info.json"), "w") as f:
        json.dump(initial_info, f, indent=2)
    
    # Generate initial HTML files
    generate_html(base_dir, run_dir, [], initial_info["command_args"])
    update_main_index(base_dir)
    
    return base_dir, run_dir

def get_default_steps(model):
    """Get default steps based on model type."""
    if model == "schnell":
        return 1
    elif model == "dev":
        return 5
    else:
        return 1  # Fallback

def generate_images(settings, run_dir):
    """Run mflux-generate command and return results."""
    results = []
    run_info = RunInfo(run_dir)
    
    # Use provided steps or default based on model
    steps = settings["steps"]
    
    # Parse resolution
    width, height = map(int, settings["resolution"].split('x'))
    
    # Get the base prompt and style
    base_prompt = settings["prompt"]
    if settings.get("style"):
        style_desc = get_style(settings["style"])
        if style_desc:
            base_prompt = f"{base_prompt}, {style_desc}"
    
    # Generate each iteration
    for i in range(settings["iterations"]):
        # Generate a random seed if not provided or if vary-seed is specified
        if settings["seed"] is not None and not settings["vary_seed"]:
            # When using --vary-steps, use the same seed for all iterations
            seed = settings["seed"]
        else:
            seed = random.randint(1, 1000000)
        
        # If using vary-steps, use the corresponding step count
        if settings["vary_steps"]:
            current_steps = settings["vary_steps_list"][i]
        else:
            current_steps = steps
        
        # Use the correct filename format with current step count
        output_path = os.path.join(run_dir, f"image_{seed}_{current_steps}.png")
        metadata_path = output_path + ".json" if settings["metadata"] else None
        
        # Build the mflux-generate command
        cmd = [
            "mflux-generate",
            "--prompt", base_prompt,  # Use the prompt with style if applicable
            "--model", settings["model"],
            "--steps", str(current_steps),
            "--seed", str(seed),
            "--width", str(width),
            "--height", str(height),
            "--output", output_path
        ]
        
        if settings["metadata"]:
            cmd.extend(["--metadata"])
        
        # Execute the command
        try:
            print(f"\nGenerating iteration {i+1}/{settings['iterations']} with seed {seed} and steps {current_steps}...")
            start_time = datetime.datetime.now()
            
            # Use Popen to capture output in real-time
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1,
                universal_newlines=True
            )
            
            # Track progress
            while True:
                output = process.stdout.readline()
                if output == '' and process.poll() is not None:
                    break
                if output:
                    # Clean and display the output
                    output = output.strip()
                    if output:  # Only print non-empty lines
                        print(f"  {output}")
            
            # Check for any errors
            rc = process.poll()
            if rc != 0:
                error = process.stderr.read()
                raise subprocess.CalledProcessError(rc, cmd, error)
            
            end_time = datetime.datetime.now()
            generation_time = (end_time - start_time).total_seconds()
            
            # Store the result with the correct filename format using current_steps
            image_info = {
                "index": i+1,
                "seed": seed,
                "steps": current_steps,
                "file_path": f"image_{seed}_{current_steps}.png",  # Use current_steps in filename
                "metadata_path": metadata_path,
                "command": " ".join(cmd),
                "generation_time": generation_time
            }
            results.append(image_info)
            
            # Update run info and HTML files after each successful generation
            run_info.add_result(image_info)
            generate_html(os.path.dirname(run_dir), run_dir, run_info.get_results(), run_info.get_settings())
            
            print(f"  ✓ Generated in {generation_time:.2f} seconds")
            
        except subprocess.CalledProcessError as e:
            print(f"Error generating image {i+1}: {e}")
            print(f"stderr: {e.stderr}")
    
    return results

def generate_html(base_dir, run_dir, results, settings):
    """Generate HTML pages for viewing the results."""
    # Create HTML for this run
    run_html_path = os.path.join(run_dir, "index.html")
    
    # Get the original prompt without style if possible
    base_prompt = settings["prompt"]
    if settings["prompt"] and settings.get("style") and "," in settings["prompt"]:
        base_prompt = settings["prompt"].split(",")[0].strip()
    
    # Format steps info
    if settings.get("vary_steps"):
        steps_info = f"Steps: {settings['vary_steps']} (varying)"
    else:
        steps = settings.get("steps", get_default_steps(settings["model"]))
        steps_info = f"Steps: {steps}"
    
    with open(run_html_path, "w") as f:
        f.write(f"""<!DOCTYPE html>
<html>
<head>
    <title>MFlux Generation Run</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        .image-container {{ display: flex; flex-wrap: wrap; gap: 20px; }}
        .image-card {{ border: 1px solid #ccc; padding: 15px; border-radius: 5px; max-width: 550px; position: relative; }}
        .image-card img {{ max-width: 100%; cursor: pointer; }}
        .metadata {{ 
            opacity: 0; 
            position: absolute; 
            bottom: 15px; 
            left: 15px; 
            right: 15px; 
            background: rgba(0,0,0,0.7); 
            color: white; 
            padding: 10px; 
            font-family: monospace; 
            transition: opacity 0.3s; 
            border-radius: 5px;
            z-index: 10;
        }}
        .image-card:hover .metadata {{ opacity: 1; }}
        .command {{ 
            opacity: 0;
            margin-top: 10px; 
            font-family: monospace; 
            background: #f0f0f0; 
            padding: 10px; 
            border-radius: 5px; 
            overflow-x: auto;
            transition: opacity 0.3s;
        }}
        .image-card:hover .command {{ opacity: 1; }}
        .command code {{ display: block; margin-top: 5px; }}
        .status {{ padding: 5px 10px; border-radius: 3px; display: inline-block; }}
        .in-progress {{ background: #fff3cd; color: #856404; }}
        .complete {{ background: #d4edda; color: #155724; }}
        h2, h3 {{ color: #333; }}
        .generation-time {{ color: #666; font-size: 0.9em; margin-top: 5px; }}
    </style>
</head>
<body>
    <h1>MFlux Generation Results</h1>
    <div class="run-info">
        <h2>Run Information</h2>
        <p><strong>Prompt:</strong> {settings["prompt"]}</p>
        <p><strong>Model:</strong> {settings["model"]}</p>
        <p><strong>Resolution:</strong> {settings["resolution"]}</p>
        <p><strong>{steps_info}</strong></p>
        <p><strong>Date:</strong> {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</p>
        <p><strong>Status:</strong> <span class="status {'complete' if len(results) == settings['iterations'] else 'in-progress'}">
            {f"Complete ({len(results)}/{settings['iterations']})" if len(results) == settings['iterations'] else f"Generating... ({len(results)}/{settings['iterations']})"}
        </span></p>
    </div>
    
    <h2>Generated Images ({len(results)})</h2>
    <div class="image-container">
""")
        
        # Add each image
        for result in results:
            # Get the image filename from the result's file_path
            file_path = result["file_path"]
            if os.path.isabs(file_path):
                image_filename = os.path.basename(file_path)
            else:
                image_filename = file_path
                
            metadata_html = ""
            
            # Add metadata if available
            metadata_path = result.get("metadata_path")
            if metadata_path and os.path.exists(metadata_path):
                try:
                    with open(metadata_path, "r") as meta_file:
                        metadata = json.load(meta_file)
                        # Format a simplified version of metadata
                        metadata_html = f"""<div class="metadata">
<strong>Seed:</strong> {result["seed"]}
<strong>Steps:</strong> {result["steps"]}
<strong>Generation Time:</strong> {result["generation_time"]:.2f} seconds
</div>"""
                except json.JSONDecodeError:
                    metadata_html = "<p>Error reading metadata.</p>"
            else:
                metadata_html = f"""<div class="metadata">
<strong>Seed:</strong> {result["seed"]}
<strong>Steps:</strong> {result["steps"]}
<strong>Generation Time:</strong> {result["generation_time"]:.2f} seconds
</div>"""
            
            # Generate commands for reusing this seed
            # Build the base command with all necessary flags
            base_cmd = f"./mflux-wrapper.py --prompt \"{base_prompt}\" --model {settings['model']} --resolution {settings['resolution']}"
            
            # Add style if it was used
            if settings.get("style"):
                base_cmd += f" --style {settings['style']}"
            
            # Add metadata flag if it was used
            if settings.get("metadata"):
                base_cmd += " --metadata"
            
            # Generate the refine and iterate commands
            refine_cmd = f"{base_cmd} --seed {result['seed']} --iterations 1 --steps {result['steps']}"
            iterate_cmd = f"{base_cmd} --seed {result['seed']} --vary-steps 1,3,5,9"
            
            f.write(f"""    <div class="image-card">
        <h3>Image {result["index"]} (Seed: {result["seed"]})</h3>
        <img src="{image_filename}" alt="Generated image {result['index']}" data-full-image="{image_filename}" />
        {metadata_html}
        <div class="command">
            <strong>Exact Reproduction Command:</strong>
            <code>{refine_cmd}</code>
            <strong>Vary Steps Command:</strong>
            <code>{iterate_cmd}</code>
        </div>
    </div>
""")
        
        f.write("""    </div>
<script>
document.querySelectorAll('.image-card img').forEach(img => {
    img.addEventListener('click', () => {
        window.open(img.getAttribute('data-full-image'), '_blank');
    });
});
</script>
</body>
</html>""")
    
    # Update or create the main index.html in the base directory
    update_main_index(base_dir)
    
    return run_html_path

def update_main_index(base_dir):
    """Update or create the main index.html that lists all runs."""
    index_path = os.path.join(base_dir, "index.html")
    
    # Get all run directories in reverse order (newest first)
    runs = sorted([d for d in os.listdir(base_dir) if d.startswith("run_")], 
                 key=lambda x: int(x.split("_")[1]), reverse=True)
    
    with open(index_path, "w") as f:
        f.write("""<!DOCTYPE html>
<html>
<head>
    <title>MFlux Generation Runs</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .run-list { display: flex; flex-direction: column; gap: 20px; }
        .run-item { border: 1px solid #ccc; padding: 15px; border-radius: 5px; }
        .image-grid { 
            display: flex; 
            flex-wrap: wrap; 
            gap: 10px; 
            margin-top: 10px; 
        }
        .image-grid a { 
            flex: 1 1 calc(33.333% - 10px);
            min-width: 250px;
            max-width: calc(33.333% - 10px);
            text-decoration: none;
            position: relative;
        }
        .image-grid img { 
            width: 100%; 
            height: auto; 
            border-radius: 3px;
            cursor: pointer;
            transition: transform 0.2s;
        }
        .image-grid a:hover img {
            transform: scale(1.02);
        }
        .image-info {
            position: absolute;
            bottom: 0;
            left: 0;
            right: 0;
            background: rgba(0,0,0,0.7);
            color: white;
            padding: 8px;
            font-size: 12px;
            opacity: 0;
            transition: opacity 0.3s;
            border-bottom-left-radius: 3px;
            border-bottom-right-radius: 3px;
        }
        .image-grid a:hover .image-info {
            opacity: 1;
        }
        .run-header { display: flex; justify-content: space-between; align-items: center; }
        h1 { color: #333; }
        h2 { margin: 0; }
        .timestamp { color: #666; }
        .status { padding: 5px 10px; border-radius: 3px; display: inline-block; margin-left: 10px; }
        .in-progress { background: #fff3cd; color: #856404; }
        .complete { background: #d4edda; color: #155724; }
        .generation-time { color: #666; font-size: 0.9em; }
        .progress-bar {
            width: 100%;
            height: 20px;
            background-color: #f0f0f0;
            border-radius: 10px;
            overflow: hidden;
            margin-top: 5px;
        }
        .progress-fill {
            height: 100%;
            background-color: #4CAF50;
            transition: width 0.3s ease;
        }
        .run-details {
            margin-top: 10px;
            padding: 10px;
            background: #f8f9fa;
            border-radius: 5px;
        }
        .run-details p {
            margin: 5px 0;
        }
    </style>
</head>
<body>
    <h1>MFlux Generation Runs</h1>
    <div class="run-list">
""")
        
        for run in runs:
            run_index = run.split("_")[1]
            run_dir = os.path.join(base_dir, run)
            run_path = os.path.join(run, "index.html")
            
            # Initialize run info
            run_info = RunInfo(run_dir)
            settings = run_info.get_settings()
            results = run_info.get_results()
            
            # Calculate total generation time
            total_time = sum(result.get("generation_time", 0) for result in results)
            time_info = f"<p class='generation-time'>Total generation time: {total_time:.2f} seconds</p>" if total_time > 0 else ""
            
            # Format steps info
            if settings.get('vary_steps'):
                steps_display = f"{settings['vary_steps']} (varying)"
            else:
                steps = settings.get('steps', get_default_steps(settings.get('model', 'schnell')))
                steps_display = str(steps)
            
            # Calculate progress
            current_images = run_info.get_current_images()
            total_iterations = run_info.get_total_iterations()
            progress = (current_images / total_iterations * 100) if total_iterations > 0 else 0
            
            # Create progress bar
            progress_bar = f"""
            <div class="progress-bar">
                <div class="progress-fill" style="width: {progress}%"></div>
            </div>
            <p>Progress: {current_images}/{total_iterations} ({progress:.1f}%)</p>
            """
            
            # Create detailed run info
            run_details = f"""
            <div class="run-details">
                <p><strong>Prompt:</strong> "{settings.get('prompt', 'N/A')}"</p>
                <p><strong>Steps:</strong> {steps_display}</p>
                {f'<p><strong>Style:</strong> {settings.get("style", "N/A")}</p>' if settings.get("style") else ""}
                {time_info}
                {progress_bar}
            </div>
            """
            
            # Create status HTML
            is_complete = run_info.is_complete()
            status_html = f"""<span class="status {'complete' if is_complete else 'in-progress'}">
                {f"Complete ({current_images}/{total_iterations})" if is_complete else f"Generating... ({current_images}/{total_iterations})"}
            </span>"""
            
            # Get all PNG images in the run directory
            images = sorted([f for f in os.listdir(run_dir) if f.endswith('.png')])
            image_grid = ""
            if images:
                image_grid = '<div class="image-grid">'
                for img in images:
                    img_path = os.path.join(run, img)
                    # Try to get image metadata from results
                    img_info = ""
                    for result in results:
                        if os.path.basename(result.get('file_path', '')) == img:
                            img_info = f"Seed: {result.get('seed', 'N/A')} | Steps: {result.get('steps', 'N/A')}"
                            break
                    
                    image_grid += f'<a href="{run_path}"><img src="{img_path}" alt="{img}" loading="lazy"><div class="image-info">{img_info}</div></a>'
                image_grid += '</div>'
            
            f.write(f"""        <div class="run-item">
            <div class="run-header">
                <h2>Run {run_index}{status_html}</h2>
                <span class="timestamp">{datetime.datetime.fromtimestamp(os.path.getctime(run_dir)).strftime('%Y-%m-%d %H:%M:%S')}</span>
            </div>
            {run_details}
            <a href="{run_path}">View Details</a>
            {image_grid}
        </div>
""")
        
        f.write("""    </div>
</body>
</html>""")

def check_live_server():
    """Check if live-server is available."""
    try:
        result = subprocess.run(['pnpx', 'live-server', '--version'], 
                              stdout=subprocess.PIPE, 
                              stderr=subprocess.PIPE,
                              text=True,
                              timeout=2)  # Timeout after 2 seconds
        return result.returncode == 0
    except (subprocess.SubprocessError, FileNotFoundError):
        return False

def save_brainstorm_results(concept, prompts, base_dir):
    """Save brainstorm results to a file in the session directory."""
    results_file = os.path.join(base_dir, "brainstorm_results.json")
    try:
        # Load existing results
        if os.path.exists(results_file):
            with open(results_file, 'r') as f:
                results = json.load(f)
        else:
            results = {}
        
        # Add new results
        results[concept] = {
            "timestamp": datetime.datetime.now().isoformat(),
            "prompts": prompts
        }
        
        # Save updated results
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2)
            
        print(f"\nBrainstorm results saved to {results_file}")
        return True
    except Exception as e:
        print(f"Error saving brainstorm results: {e}")
        return False

def load_brainstorm_results(base_dir):
    """Load the most recent brainstorm results from the session directory."""
    results_file = os.path.join(base_dir, "brainstorm_results.json")
    if not os.path.exists(results_file):
        print("No brainstorm results found.")
        return None
        
    try:
        with open(results_file, 'r') as f:
            results = json.load(f)
            if not results:
                print("No brainstorm results found.")
                return None
                
            # Get the most recent concept
            latest_concept = max(results.items(), key=lambda x: x[1]["timestamp"])[0]
            return {
                "concept": latest_concept,
                "prompts": results[latest_concept]["prompts"]
            }
    except Exception as e:
        print(f"Error loading brainstorm results: {e}")
        return None

def get_or_create_session(args):
    """Get the current session directory or create a new one if needed."""
    now = datetime.datetime.now()
    session_dirs = []
    
    # Find all session directories
    for d in os.listdir('.'):
        if d.startswith('mflux_output_'):
            try:
                dir_time = datetime.datetime.strptime(d.split('_', 2)[2], "%Y%m%d_%H%M%S")
                session_dirs.append((dir_time, d))
            except ValueError:
                continue
    
    if not session_dirs or args.new:
        # Create new session directory
        timestamp = now.strftime("%Y%m%d_%H%M%S")
        base_dir = f"mflux_output_{timestamp}"
        os.makedirs(base_dir, exist_ok=True)
        return base_dir
    
    # Get most recent session
    latest_session = sorted(session_dirs, reverse=True)[0]
    latest_time, latest_dir = latest_session
    
    # Check if session is still valid (less than 4 hours old)
    if (now - latest_time).total_seconds() < 14400:  # 4 hours in seconds
        return latest_dir
    else:
        # Create new session directory
        timestamp = now.strftime("%Y%m%d_%H%M%S")
        base_dir = f"mflux_output_{timestamp}"
        os.makedirs(base_dir, exist_ok=True)
        return base_dir

def brainstorm_prompts(concept, args):
    """Generate prompt ideas using Claude via llm command."""
    # Check if llm command is available
    try:
        subprocess.run(['llm', '--version'], capture_output=True, check=True)
    except (subprocess.CalledProcessError, FileNotFoundError):
        print("Error: 'llm' command not found. Please install it first:")
        print("  pip install llm")
        return

    # Create a new session directory for the brainstorm
    base_dir = get_or_create_session(args)
    
    # Generate prompts using Claude
    prompt = f"give me 5 variations of this idea, that describes a close up image representing {concept}. these ideas will be fed into an image prompt."
    try:
        result = subprocess.run(
            ['llm', '-m', 'claude-3.7-sonnet', prompt],
            capture_output=True,
            text=True,
            check=True
        )
        # Split into lines and clean up
        raw_lines = result.stdout.strip().split('\n')
        prompts = []
        
        # Process each line
        for line in raw_lines:
            line = line.strip()
            if not line:  # Skip empty lines
                continue
            # Skip lines that don't start with a number followed by a dot
            if not (line[0].isdigit() and '. ' in line):
                continue
            # Remove the number and dot, then clean up
            prompt = line.split('. ', 1)[1].strip()
            prompts.append(prompt)
        
        # Ensure we have exactly 5 prompts
        if len(prompts) > 5:
            prompts = prompts[:5]
        elif len(prompts) < 5:
            print(f"Warning: Only got {len(prompts)} prompts from Claude")
        
        # Save prompts to a file
        prompts_file = os.path.join(base_dir, "prompt-ideas.txt")
        with open(prompts_file, 'w') as f:
            for i, prompt in enumerate(prompts, 1):
                f.write(f"{i}. {prompt}\n")
        
        # Save prompts to brainstorm_results.json
        save_brainstorm_results(concept, prompts, base_dir)
        
        # Print prompts to console
        print(f"\nGenerated prompts for concept: {concept}")
        print("\nPrompts:")
        for i, prompt in enumerate(prompts, 1):
            print(f"{i}. {prompt}\n")
        
        print(f"\nPrompts saved to: {prompts_file}")
        print("\nTo run specific prompts, use:")
        print(f"  ./mflux-wrapper.py --run-prompt INDEX")
        print("Example: --run-prompt 2")
        
    except subprocess.CalledProcessError as e:
        print(f"Error running llm command: {e}")
        print("Make sure you have the llm command installed and configured with Claude API access")
    except Exception as e:
        print(f"Error generating prompts: {e}")

def main():
    """Main function to execute the tool."""
    args = parse_args()
    
    # Handle brainstorm commands
    if args.brainstorm:
        brainstorm_prompts(args.brainstorm, args)
        return
    
    # Get or create session directory first
    base_dir = get_or_create_session(args)
    
    # Load settings from last run
    settings = load_last_settings(base_dir)
    
    # Handle run-prompt command
    if args.run_prompt:
        # Load the most recent brainstorm results
        brainstorm_data = load_brainstorm_results(base_dir)
        if not brainstorm_data:
            print("Error: No brainstorm results found. Please run --brainstorm first.")
            return
            
        prompts = brainstorm_data["prompts"]
        if not prompts:
            print("Error: No prompts found in brainstorm results.")
            return
            
        # Validate the prompt index
        if args.run_prompt < 1 or args.run_prompt > len(prompts):
            print(f"Error: Invalid prompt index. Please choose between 1 and {len(prompts)}.")
            return
            
        # Get the selected prompt
        selected_prompt = prompts[args.run_prompt - 1]
        print(f"\nRunning prompt {args.run_prompt}:")
        print(f"Prompt: {selected_prompt}")
        
        # Set the prompt in settings
        settings["prompt"] = selected_prompt
    
    # Apply command line arguments to settings
    if args.prompt is not None:
        settings["prompt"] = args.prompt
        if hasattr(args, 'style_desc'):
            settings["prompt"] = f"{settings['prompt']}, {args.style_desc}"
    
    # Always update model setting if specified (remove the schnell check)
    settings["model"] = args.model
    
    if args.steps is not None:
        settings["steps"] = args.steps
    
    if args.resolution != "1024x1024":
        settings["resolution"] = args.resolution
    
    if args.iterations != 4:
        settings["iterations"] = args.iterations
    
    if args.metadata:
        settings["metadata"] = True
    
    if args.vary_seed:
        settings["vary_seed"] = True
    
    if args.seed is not None:
        settings["seed"] = args.seed
    
    if hasattr(args, 'style'):
        settings["style"] = args.style
    
    # Handle vary-steps settings
    if hasattr(args, 'vary_steps_list'):
        settings["vary_steps"] = args.vary_steps
        settings["vary_steps_list"] = args.vary_steps_list
        settings["iterations"] = len(args.vary_steps_list)  # Override iterations to match vary-steps list
        settings["steps"] = None  # Clear any manually set steps
        # Make sure we preserve the model setting
        print(f"\nUsing model: {settings['model']} with varying steps: {settings['vary_steps']}")
    
    # Validate required settings
    if settings["prompt"] is None:
        raise ValueError("No prompt specified and couldn't find last prompt in current session")
    
    # Create run directory
    existing_runs = [d for d in os.listdir(base_dir) if d.startswith("run_")]
    if existing_runs:
        run_numbers = [int(run.split("_")[1]) for run in existing_runs]
        next_run = max(run_numbers) + 1
    else:
        next_run = 1
    
    run_dir = os.path.join(base_dir, f"run_{next_run}")
    os.makedirs(run_dir, exist_ok=True)
    
    # Initialize run info
    run_info = RunInfo(run_dir)
    run_info.update_settings(settings)
    
    # Generate initial HTML files with the settings
    generate_html(base_dir, run_dir, [], settings)
    update_main_index(base_dir)
    
    print(f"Created run directory: {run_dir}")
    
    # Start live-server if watching is enabled and available
    live_server_process = None
    if not args.no_watch:
        if check_live_server():
            try:
                print(f"\nStarting live-server for {base_dir}...")
                live_server_process = subprocess.Popen(['pnpx', 'live-server', base_dir])
                print("Live-server started. Your browser will refresh automatically as images are generated.")
            except Exception as e:
                print(f"\nWarning: Could not start live-server: {e}")
                live_server_process = None
        else:
            print("\nLive-server not found. To enable live preview, install it with:")
            print("  npm install -g live-server")
    
    try:
        # Generate images
        results = generate_images(settings, run_dir)
        
        # Final HTML update is already done in generate_images
        print(f"\nGeneration complete!")
        print(f"Generated {len(results)} iterations")
        print(f"Results saved to: {run_dir}")
        print(f"View results: {os.path.join(run_dir, 'index.html')}")
        
        if live_server_process:
            print("\nStopping live-server...")
            try:
                # First try graceful termination
                live_server_process.terminate()
                try:
                    # Wait for up to 5 seconds for graceful termination
                    live_server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    # If still running after 5 seconds, force kill
                    print("Live-server not responding to terminate, forcing kill...")
                    live_server_process.kill()
                    try:
                        # Wait for up to 2 more seconds for kill to take effect
                        live_server_process.wait(timeout=2)
                    except subprocess.TimeoutExpired:
                        print("Warning: Could not confirm live-server termination")
                print("Live-server stopped.")
            except Exception as e:
                print(f"Warning: Error stopping live-server: {e}")
        elif not args.no_watch:
            print("\nTo monitor changes in real-time, install and run:")
            print("  npm install -g live-server")
            print(f"  pnpx live-server {base_dir}")
    
    except KeyboardInterrupt:
        print("\nGeneration interrupted!")
        if live_server_process:
            print("Stopping live-server...")
            try:
                # First try graceful termination
                live_server_process.terminate()
                try:
                    # Wait for up to 5 seconds for graceful termination
                    live_server_process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    # If still running after 5 seconds, force kill
                    print("Live-server not responding to terminate, forcing kill...")
                    live_server_process.kill()
                    try:
                        # Wait for up to 2 more seconds for kill to take effect
                        live_server_process.wait(timeout=2)
                    except subprocess.TimeoutExpired:
                        print("Warning: Could not confirm live-server termination")
                print("Live-server stopped.")
            except Exception as e:
                print(f"Warning: Error stopping live-server: {e}")
        raise

if __name__ == "__main__":
    main()

    