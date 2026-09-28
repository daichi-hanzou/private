"""Ordinary fixed-margin farmer policy; never invokes a language model."""

from coffeebench.agent import Agent
from coffeebench.models.types import ModelResponse, ToolCall


class RuleFarmerAgent(Agent):
    def __init__(self, *, business_app, **kwargs):
        super().__init__(**kwargs)
        self.ba = business_app
        if self.ba.role != "farmer":
            raise ValueError("rule_farmer can only control a farmer")

    def step_query(self):
        ba, market = self.ba, self.ba.marketplace
        env = market._env

        def action(name, **args):
            self.model.n_calls += 1  # Scripted decisions, not provider calls.
            return ModelResponse(
                tool_calls=[ToolCall(f"rule-{self.model.n_calls}", name, args)]
            )

        for inv in ba.accounts_payable:
            if (
                inv.net_outstanding > 0
                and not inv.bad_debt
                and ba.cash >= inv.net_outstanding
            ):
                return action("pay_invoice", invoice_id=inv.id)
        if getattr(env, "research", None) and env.research.stopped:
            return ModelResponse()
        for offer in market.offers:
            if offer.seller_id != ba.agent_id or offer.status != "pending":
                continue
            listing = next(x for x in market.listings if x.id == offer.listing_id)
            cost = ba.cost_basis.get(listing.item_id, 0)
            buyer = market.business_apps[offer.buyer_id]
            if (
                listing.status == "open"
                and offer.offered_price >= cost * 1.2
                and offer.qty <= min(listing.qty, ba.inventory.get(listing.item_id, 0))
                and buyer._inventory_capacity_remaining_kg() >= offer.qty
            ):
                return action("accept_offer", offer_id=offer.id)
        for item, qty in sorted(ba.inventory.items()):
            listed = sum(
                x.qty
                for x in market.listings
                if x.seller_id == ba.agent_id
                and x.item_id == item
                and x.status == "open"
            )
            if qty > listed:
                return action(
                    "post_listing",
                    item_id=item,
                    qty=qty - listed,
                    asking_price=round(ba.cost_basis.get(item, 0) * 1.5, 2),
                    payment_terms_days=0,
                )
        for item in market.items.values():
            if item.produced_by_role != "farmer":
                continue
            used = env._production_used_today.get(f"{ba.agent_id}::{item.id}", 0)
            qty = min(
                item.daily_production_cap - used,
                ba._inventory_capacity_remaining_kg(),
                int(ba.cash / item.production_cost_per_unit),
            )
            if qty > 0:
                return action("produce_item", item_id=item.id, quantity=qty)
        return ModelResponse()
