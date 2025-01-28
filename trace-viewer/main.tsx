/* @jsx h */
import { h, render } from "preact";
import { useState, useEffect, useMemo } from "preact/hooks";
import * as d3 from "d3";

interface Event {
  type: string;
  node?: number;
  dependencies?: number[];
  attr?: string;
  value?: any;
  from?: number;
  to?: number;
}

interface NodeData {
  id: number;
  name?: string;
  attributes: Record<string, any>;
  x?: number;
  y?: number;
}

interface GraphState {
  nodes: Record<number, NodeData>;
  edges: Set<string>;
}

const processEvents = (events: Event[]): GraphState[] => {
  const states: GraphState[] = [];
  let current: GraphState = { nodes: {}, edges: new Set() };

  let attrBuffer: Record<number, Record<string, any>> = {};
  for (const event of events) {
    if (event.type === "new") {
      current = {
        nodes: {
          ...current.nodes,
          [event.node!]: { id: event.node!, attributes: {} },
        },
        edges: new Set([
          ...current.edges,
          ...event.dependencies!.map((dep) => `${event.node}-${dep}`),
        ]),
      };
    } else if (event.type === "attrChange") {
      if (!attrBuffer[event.node!]) attrBuffer[event.node!] = {};
      attrBuffer[event.node!][event.attr!] = event.value;
    } else if (event.type === "edgeAdd") {
      current = {
        ...current,
        edges: new Set([...current.edges, `${event.from}-${event.to}`]),
      };
    } else if (event.type === "edgeRemove") {
      const newEdges = new Set(current.edges);
      newEdges.delete(`${event.from}-${event.to}`);
      current = { ...current, edges: newEdges };
    }

    if (Object.keys(attrBuffer).length > 0) {
      const newNodes = { ...current.nodes };
      for (const nodeId in attrBuffer) {
        newNodes[nodeId] = {
          ...newNodes[nodeId],
          attributes: {
            ...newNodes[nodeId]?.attributes,
            ...attrBuffer[nodeId],
          },
        };
      }
      attrBuffer = {};
      states.push({ nodes: newNodes, edges: current.edges });
    } else {
      states.push(current);
    }
  }
  return states;
};

function GraphViewer({ events }: { events: Event[] }) {
  const [step, setStep] = useState(0);
  const states = useMemo(() => processEvents(events), [events]);
  const graph = useMemo(() => states[step], [step, states]);

  useEffect(() => {
    const svg = d3.select("#graph");
    const nodes = Object.values(graph.nodes);
    const links = [...graph.edges].map((e) => {
      const [from, to] = e.split("-").map(Number);
      return { source: from, target: to };
    });

    d3.forceSimulation(nodes)
      .force(
        "link",
        d3.forceLink(links).id((d: any) => d.id)
      )
      .force("charge", d3.forceManyBody())
      .force("center", d3.forceCenter(300, 300));

    const link = svg.selectAll("line").data(links);
    link
      .enter()
      .append("line")
      .attr("stroke", "#999")
      .attr("marker-end", "url(#arrowhead)")
      .merge(link as any)
      .transition()
      .duration(500)
      .attr("x1", (d: any) => d.source.x)
      .attr("y1", (d: any) => d.source.y)
      .attr("x2", (d: any) => d.target.x)
      .attr("y2", (d: any) => d.target.y);
    link.exit().remove();

    const node = svg.selectAll("circle").data(nodes);
    node
      .enter()
      .append("circle")
      .attr("r", 10)
      .attr("fill", "#69b3a2")
      .merge(node as any)
      .transition()
      .duration(500)
      .attr("cx", (d: any) => d.x)
      .attr("cy", (d: any) => d.y);
    node.exit().remove();
  }, [graph]);

  return (
    <div className="flex">
      <div className="w-1/4 h-600 overflow-y-auto border-r p-2">
        <ul>
          {states.map((_, index) => (
            <li
              key={index}
              className={`cursor-pointer p-1 ${
                index === step ? "bg-gray-300" : ""
              }`}
              onClick={() => setStep(index)}
            >
              Step {index + 1}
            </li>
          ))}
        </ul>
      </div>
      <div className="w-3/4 p-2">
        <svg id="graph" width={600} height={600}></svg>
        <div className="flex gap-2 mt-2">
          <button
            onClick={() => setStep((s) => Math.max(0, s - 1))}
            className="px-4 py-2 bg-gray-200 rounded"
          >
            Prev
          </button>
          <button
            onClick={() => setStep((s) => Math.min(states.length - 1, s + 1))}
            className="px-4 py-2 bg-gray-200 rounded"
          >
            Next
          </button>
        </div>
      </div>
    </div>
  );
}

fetch("./trace.jsonl").then(async (res) => {
  const events: Event[] = await res.text().then((text) =>
    text.trim().split("\n").map((x, index) => {
      try {
        return JSON.parse(x);
      } catch (e) {
        console.log("Error at line", index, e);
        throw e;
      }
    })
  );
  render(<GraphViewer events={events} />, document.getElementById("app")!);
});
