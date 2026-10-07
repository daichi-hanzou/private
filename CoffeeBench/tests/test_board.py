import json
from pathlib import Path
from types import SimpleNamespace
import pytest
from coffeebench import main
from coffeebench.config import RunConfig
from coffeebench.trade_judge import build_packets
from coffeebench.lot_report import build_report_data

ROOT=Path(__file__).resolve().parents[1]


@pytest.fixture
def board_env(monkeypatch,tmp_path):
    monkeypatch.chdir(tmp_path)
    config=ROOT/'experiments/minimal/revenue_demand20_day4_12days_public_targets_azure.toml'
    env,path,_=main.build_run(SimpleNamespace(config=str(config),model='passive',models=None,
                                            seed=0,max_days=None,main_agent=None))
    yield env,path
    if env.event_logger: env.event_logger.close()


def test_board_public_reply_read_tracking_and_validation(board_env):
    env,_=board_env
    apps=env.business_apps
    a,b=apps['retailer_A'],apps['farmer_A']
    before=len(env.marketplace.deals)
    root=a.post_board_message('共同提案','全社向けです')
    assert root['status']=='success'
    assert a.view_board()['posts']==[]
    assert 'Unread public board posts: 1' in env._format_observation('farmer_A',0,None)
    assert len(b.view_board()['posts'])==1
    assert 'Unread public board posts: 0' in env._format_observation('farmer_A',0,None)
    assert len(apps['roaster_A'].view_board()['posts'])==1
    reply=b.post_board_message('返信','詳細を教えてください',root['post_id'])
    assert reply['thread_id']==root['post_id']
    assert len(a.view_board(thread_id=root['thread_id'])['posts'])==1
    assert len(a.view_board(unread_only=False,thread_id=root['thread_id'])['posts'])==2
    assert b.post_board_message('bad','body','missing')['status']=='error'
    assert b.post_board_message('x'*81,'body')['status']=='error'
    assert b.post_board_message('title','x'*4001)['status']=='error'
    assert b.view_board(limit=0)['status']=='error'
    assert len(env.marketplace.deals)==before
    for agent in env.agents.values(): assert 'public board is available' in agent.system_prompt
    for app in apps.values():
        assert {'post_board_message','view_board'} <= {t.__name__ for t in main._collect_tools(app)}


def test_board_pagination_and_snapshot(board_env):
    env,path=board_env
    a,b=env.business_apps['retailer_A'],env.business_apps['retailer_B']
    for i in range(3): a.post_board_message(str(i),'hello')
    first=b.view_board(limit=1)
    assert first['has_more']
    first['posts'][0]['body']='tampered'
    assert env.marketplace.board_posts[0]['body']=='hello'
    assert len(b.view_board(limit=10)['posts'])==2
    assert not b.view_board()['posts']
    env.save_trajectory(path)
    run=json.loads(Path(path).read_text(encoding='utf-8'))
    assert len(run['marketplace']['board_posts'])==3
    assert len(run['marketplace']['board_read_ids']['retailer_B'])==3
    packets=build_packets(run)
    assert len(packets)==15
    assert all(len(p['messages'])==3 for p in packets)
    assert all(p['recipient_id']=='all' for p in build_report_data(run)['messages'])


def test_disabled_tools_and_config_validation(monkeypatch,tmp_path):
    monkeypatch.chdir(tmp_path)
    config=ROOT/'experiments/minimal/revenue_normal.toml'
    env,_,_=main.build_run(SimpleNamespace(config=str(config),model='passive',models=None,
                                         seed=0,max_days=None,main_agent=None))
    try:
        a=env.business_apps['farmer_A']
        assert 'post_board_message' not in {t.__name__ for t in main._collect_tools(a)}
        assert a.post_board_message('hi','hello')['status']=='error'
        assert a.view_board()['status']=='error'
        assert 'Unread public board' not in env._format_observation('farmer_A',0,None)
        assert 'public board is available' not in env.agents['farmer_A'].system_prompt
    finally:
        if env.event_logger: env.event_logger.close()
    bad=tmp_path/'bad.toml';bad.write_text('[run]\npublic_board_enabled="yes"\n')
    with pytest.raises(ValueError): RunConfig.from_toml(bad)


def test_board_watch_and_time_cost(monkeypatch,capsys):
    from coffeebench.watch import _stream
    from coffeebench.event_loop import TOOL_TIME_COST_MIN
    assert TOOL_TIME_COST_MIN['post_board_message']==30
    assert TOOL_TIME_COST_MIN['view_board']==30
    monkeypatch.setattr(_stream,'seen',0,raising=False)
    _stream([dict(type='board_posted',sender_id='farmer_A',sent_at=1500,
                  title='公開',body='全社への提案',reply_to=None)],False,True)
    assert 'BOARD: farmer_A -> ALL' in capsys.readouterr().out
