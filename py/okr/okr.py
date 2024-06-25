"""
An OKR consists of an **Objective**, which tells you where to go. 
Several **Key Results**, which are the measurable results you need to achieve
to get to your Objective. And **Initiatives**, which are all the projects and
tasks that will help you achieve your Key Results.
"""

from typing import List, Optional

from sqlalchemy import ForeignKey, String, create_engine
from sqlalchemy.orm import (DeclarativeBase, Mapped, Session, mapped_column,
                            relationship)


class Base(DeclarativeBase):
    @property
    def idx(self):
        return self.__class__.__name__.lower()[0] + str(self.id)

class Objective(Base):
    __tablename__ = "objective"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str] = mapped_column(String(30))
    keyresults: Mapped[List["KeyResult"]] = relationship(
        back_populates="objective", cascade="all, delete-orphan"
    )

    def __repr__(self) -> str:
        return f"Objective(id={self.id!r}, name={self.name!r})"



class KeyResult(Base):
    __tablename__ = "keyresult"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str]
    objective_id: Mapped[int] = mapped_column(ForeignKey("objective.id"))
    objective: Mapped["Objective"] = relationship(back_populates="keyresults")
    initiatives: Mapped[List["Initiative"]] = relationship(
        back_populates="keyresult", cascade="all, delete-orphan"
    )

    def __repr__(self) -> str:
        return f"KeyResult(id={self.id!r}, name={self.name!r})"


class Initiative(Base):
    __tablename__ = "initiative"

    id: Mapped[int] = mapped_column(primary_key=True)
    name: Mapped[str]
    keyresult_id: Mapped[int] = mapped_column(ForeignKey("keyresult.id"))
    keyresult: Mapped["KeyResult"] = relationship(back_populates="initiatives")

    def __repr__(self) -> str:
        return f"Initiative(id={self.id!r}, name={self.name!r})"

def mk_graph(*objectives):
    import graphviz
    g = graphviz.Digraph('okr', comment='objectives / keyresults', node_attr=dict(
            style="rounded,filled", shape='box'
        ), graph_attr=dict(rankdir="LR"))
    for obj in objectives:
        g.node(obj.idx, obj.name, fillcolor="aqua")
        for kr in obj.keyresults:
            g.node(kr.idx, kr.name, fillcolor="gainsboro")
            g.edge(kr.idx, obj.idx)
            for initiative in kr.initiatives:
                g.node(initiative.idx, initiative.name, fillcolor="aliceblue")
                g.edge(initiative.idx, kr.idx)
    g.render(directory='graph', view=True)



def test():
    engine = create_engine("sqlite://", echo=True)
    Base.metadata.create_all(engine)
    with Session(engine) as session:
        obj1 = Objective(name="Crush the competition through acquisitions")
        kr1 = KeyResult(name="Acquire 3 small players in our industry")
        in1 = Initiative(name="Secure M&A financing approval from banks")
        kr1.initiatives = [in1]
        obj1.keyresults = [kr1]
        session.add_all([obj1, kr1, in1])
        session.commit()
        mk_graph(obj1)

if __name__ == '__main__':
    test()
