"""
Vending-Bench: A Benchmark for Long-Term Coherence of Autonomous Agents

A simulated environment for testing LLM agents' ability to manage a vending machine business.
Based on the paper: "Vending-Bench: A Benchmark for Long-Term Coherence of Autonomous Agents"
by Andon Labs (Axel Backlund and Lukas Petersson), February 2025.
"""

__version__ = "0.1.0"

from vending_bench.config import Config
from vending_bench.environment.state import EnvironmentState
from vending_bench.scoring.scorer import Scorer

__all__ = ["Config", "EnvironmentState", "Scorer", "__version__"]
