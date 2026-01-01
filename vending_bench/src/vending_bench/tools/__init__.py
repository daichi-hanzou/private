"""Tools for Vending-Bench agents."""

from vending_bench.tools.base import BaseTool, ToolResult, ToolRegistry
from vending_bench.tools.main_agent_tools import (
    ReadEmailsTool,
    ReadEmailTool,
    ReadEmailInboxTool,
    SendEmailTool,
    AIWebSearchTool,
    GetStorageInventoryTool,
    CheckStorageQuantitiesTool,
    ListStorageProductsTool,
    GetMoneyBalanceTool,
    WaitForNextDayTool,
    SubAgentSpecsTool,
    RunSubAgentTool,
    ChatWithSubAgentTool,
)
from vending_bench.tools.sub_agent_tools import (
    StockProductsTool,
    CollectCashTool,
    SetPricesTool,
    GetMachineInventoryTool,
)
from vending_bench.tools.memory_tools import (
    WriteScratchpadTool,
    ReadScratchpadTool,
    SetKVValueTool,
    GetKVValueTool,
    DeleteKVValueTool,
    AddToVectorDBTool,
    SearchVectorDBTool,
)

__all__ = [
    "BaseTool",
    "ToolResult",
    "ToolRegistry",
    # Main agent tools
    "ReadEmailsTool",
    "ReadEmailTool",
    "ReadEmailInboxTool",
    "SendEmailTool",
    "AIWebSearchTool",
    "GetStorageInventoryTool",
    "CheckStorageQuantitiesTool",
    "ListStorageProductsTool",
    "GetMoneyBalanceTool",
    "WaitForNextDayTool",
    "SubAgentSpecsTool",
    "RunSubAgentTool",
    "ChatWithSubAgentTool",
    # Sub-agent tools
    "StockProductsTool",
    "CollectCashTool",
    "SetPricesTool",
    "GetMachineInventoryTool",
    # Memory tools
    "WriteScratchpadTool",
    "ReadScratchpadTool",
    "SetKVValueTool",
    "GetKVValueTool",
    "DeleteKVValueTool",
    "AddToVectorDBTool",
    "SearchVectorDBTool",
]
