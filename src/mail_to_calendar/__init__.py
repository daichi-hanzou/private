"""Local rule-based and optional Ollama-assisted mail analysis."""

from .extractor import RuleBasedCalendarExtractor, to_calendar_request
from .hybrid_analyzer import HybridMailAnalyzer
from .llm_classifier import LLMCalendarClassifier
from .llm_models import LLMAnalysisInput, LLMAnalysisResult
from .models import CalendarCandidate, EmailMessage, ImportanceResult
from .ollama_client import OllamaClient
from .service import MailToCalendarService, ProcessingResult

__all__ = [
    "CalendarCandidate",
    "EmailMessage",
    "ImportanceResult",
    "HybridMailAnalyzer",
    "LLMAnalysisInput",
    "LLMAnalysisResult",
    "LLMCalendarClassifier",
    "MailToCalendarService",
    "ProcessingResult",
    "OllamaClient",
    "RuleBasedCalendarExtractor",
    "to_calendar_request",
]
