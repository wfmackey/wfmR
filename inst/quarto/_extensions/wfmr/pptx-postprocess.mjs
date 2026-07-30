#!/usr/bin/env node

// Add presentation-level chrome that PowerPoint cannot derive from a master:
// an arbitrary Quarto footer, current/total numbering, and a proportional
// progress fill. The reference deck still provides the native footer and
// slide-number placeholders, so Reset and Apply Layout remain useful.

import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";

const GENERATED_NAMES = ["wfmr-progress-fill", "wfmR generated progress fill"];
const SLIDE_WIDTH = 12192000;
const PROGRESS_Y = 6829425;
const PROGRESS_HEIGHT = 28575;
const NUMBER_X = 11334750;
const NUMBER_Y = 6515100;
const NUMBER_WIDTH = 762000;
const NUMBER_HEIGHT = 247650;

function xmlEscape(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&apos;");
}

function xmlUnescape(value) {
  return String(value ?? "")
    .replaceAll("&apos;", "'")
    .replaceAll("&quot;", '"')
    .replaceAll("&gt;", ">")
    .replaceAll("&lt;", "<")
    .replaceAll("&amp;", "&");
}

function shapeBlocks(xml) {
  return xml.match(/<p:sp>(?:(?!<p:sp>|<\/p:sp>)[\s\S])*?<\/p:sp>/g) ?? [];
}

function replaceShape(xml, predicate, updater) {
  for (const block of shapeBlocks(xml)) {
    if (predicate(block)) return xml.replace(block, updater(block));
  }
  return xml;
}

function replaceRequiredShape(xml, predicate, updater, label) {
  for (const block of shapeBlocks(xml)) {
    if (predicate(block)) return xml.replace(block, updater(block));
  }
  throw new Error(`Missing ${label} placeholder`);
}

function removeNamedShapes(xml, names) {
  for (const block of shapeBlocks(xml)) {
    if (names.some((name) => block.includes(`name="${name}"`))) {
      xml = xml.replace(block, "");
    }
  }
  return xml;
}

function replaceFirstText(block, value) {
  const escaped = xmlEscape(value);
  if (/<a:t(?:\s[^>]*)?>[\s\S]*?<\/a:t>/.test(block)) {
    return block.replace(/<a:t(?:\s[^>]*)?>[\s\S]*?<\/a:t>/, `<a:t xml:space="preserve">${escaped}</a:t>`);
  }
  return block;
}

function forceRunThemeColor(block) {
  return block.replace(/<a:rPr\b[^>]*>[\s\S]*?<\/a:rPr>/g, (runProperties) => {
    const themeFill = '<a:solidFill><a:schemeClr val="tx2"/></a:solidFill>';
    if (/<a:solidFill>[\s\S]*?<\/a:solidFill>/.test(runProperties)) {
      return runProperties.replace(/<a:solidFill>[\s\S]*?<\/a:solidFill>/, themeFill);
    }
    return runProperties.replace("</a:rPr>", `${themeFill}</a:rPr>`);
  });
}

function setShapeGeometry(block, {x, y, width, height}) {
  const transform = `<a:xfrm><a:off x="${x}" y="${y}"/><a:ext cx="${width}" cy="${height}"/></a:xfrm>`;
  if (/<a:xfrm>[\s\S]*?<\/a:xfrm>/.test(block)) {
    return block.replace(/<a:xfrm>[\s\S]*?<\/a:xfrm>/, transform);
  }
  if (/<p:spPr\s*\/>/.test(block)) {
    return block.replace(/<p:spPr\s*\/>/, `<p:spPr>${transform}</p:spPr>`);
  }
  return block.replace(/<p:spPr\b[^>]*>/, (opening) => `${opening}${transform}`);
}

function replaceSlideNumber(block, value) {
  const escaped = xmlEscape(value);
  let updated = block.replace(/<a:fld\b[^>]*>[\s\S]*?<\/a:fld>/, (field) => {
    const runProperties = field.match(/<a:rPr\b[^>]*>[\s\S]*?<\/a:rPr>/)?.[0]
      ?? '<a:rPr lang="en-AU" sz="1200"><a:solidFill><a:schemeClr val="tx2"/></a:solidFill><a:latin typeface="+mn-lt"/><a:ea typeface="+mn-lt"/><a:cs typeface="+mn-lt"/></a:rPr>';
    return `<a:r>${runProperties}<a:t xml:space="preserve">${escaped}</a:t></a:r>`;
  });
  if (updated === block) updated = replaceFirstText(updated, value);
  updated = forceRunThemeColor(updated);
  return setShapeGeometry(updated, {
    x: NUMBER_X,
    y: NUMBER_Y,
    width: NUMBER_WIDTH,
    height: NUMBER_HEIGHT,
  });
}

function progressShape(id, current, total) {
  const ratio = total > 1 ? (current - 1) / (total - 1) : 0;
  const width = Math.round(SLIDE_WIDTH * ratio);
  if (width <= 0) return "";
  return `<p:sp><p:nvSpPr><p:cNvPr id="${id}" name="wfmr-progress-fill"/><p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr><a:xfrm><a:off x="0" y="${PROGRESS_Y}"/><a:ext cx="${width}" cy="${PROGRESS_HEIGHT}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom><a:solidFill><a:schemeClr val="accent1"/></a:solidFill><a:ln w="0"><a:noFill/></a:ln></p:spPr></p:sp>`;
}

function maxShapeId(xml) {
  let maximum = 1;
  for (const match of xml.matchAll(/<p:cNvPr\b[^>]*\bid="(\d+)"/g)) {
    maximum = Math.max(maximum, Number(match[1]));
  }
  return maximum;
}

function customProperty(xml, name) {
  const escapedName = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const pattern = new RegExp(`<property\\b[^>]*\\bname="${escapedName}"[^>]*>[\\s\\S]*?<vt:lpwstr>([\\s\\S]*?)<\\/vt:lpwstr>[\\s\\S]*?<\\/property>`);
  const value = xml.match(pattern)?.[1];
  return value === undefined ? "" : xmlUnescape(value);
}

function slideOrder(presentationXml, relationshipsXml) {
  const relationships = new Map();
  for (const match of relationshipsXml.matchAll(/<Relationship\b[^>]*\bId="([^"]+)"[^>]*\bTarget="([^"]+)"[^>]*\/?>(?:<\/Relationship>)?/g)) {
    relationships.set(match[1], match[2]);
  }
  const slides = [];
  for (const match of presentationXml.matchAll(/<p:sldId\b[^>]*\br:id="([^"]+)"[^>]*\/?>(?:<\/p:sldId>)?/g)) {
    const target = relationships.get(match[1]);
    if (target?.startsWith("slides/")) slides.push(target);
  }
  return slides;
}

async function patchPresentation(pptxPath) {
  const unpacked = await fs.mkdtemp(path.join(os.tmpdir(), "wfmr-pptx-postprocess-"));
  const replacement = `${pptxPath}.wfmr-${process.pid}.tmp`;
  try {
    execFileSync("/usr/bin/unzip", ["-q", "-o", pptxPath, "-d", unpacked]);
    const customXml = await fs.readFile(path.join(unpacked, "docProps/custom.xml"), "utf8").catch(() => "");
    const footer = customProperty(customXml, "wfmr-footer");
    const presentationXml = await fs.readFile(path.join(unpacked, "ppt/presentation.xml"), "utf8");
    const relationshipsXml = await fs.readFile(path.join(unpacked, "ppt/_rels/presentation.xml.rels"), "utf8");
    const slides = slideOrder(presentationXml, relationshipsXml);
    if (slides.length === 0) throw new Error(`No slides found in ${pptxPath}`);

    for (let index = 0; index < slides.length; index += 1) {
      const slidePath = path.join(unpacked, "ppt", slides[index]);
      let xml = await fs.readFile(slidePath, "utf8");
      xml = removeNamedShapes(xml, GENERATED_NAMES);
      if (footer) {
        xml = replaceRequiredShape(
          xml,
          (block) => /name="Footer Placeholder(?: \d+)?"/.test(block),
          (block) => forceRunThemeColor(replaceFirstText(block, footer)),
          "footer",
        );
      }
      xml = replaceRequiredShape(
        xml,
        (block) => /name="Slide Number Placeholder(?: \d+)?"/.test(block),
        (block) => replaceSlideNumber(block, `${index + 1} / ${slides.length}`),
        "slide-number",
      );
      const progress = progressShape(maxShapeId(xml) + 1, index + 1, slides.length);
      if (progress) xml = xml.replace("</p:spTree>", `${progress}</p:spTree>`);
      await fs.writeFile(slidePath, xml);
    }

    await fs.rm(replacement, {force: true});
    execFileSync("/usr/bin/zip", ["-q", "-X", "-r", replacement, "."], {cwd: unpacked});
    execFileSync("/usr/bin/unzip", ["-tq", replacement]);
    await fs.rename(replacement, pptxPath);
    process.stdout.write(`wfmR PowerPoint chrome: ${path.basename(pptxPath)} (${slides.length} slides)\n`);
  } finally {
    await fs.rm(unpacked, {recursive: true, force: true});
    await fs.rm(replacement, {force: true});
  }
}

const supplied = process.argv.slice(2);
const outputFiles = supplied.length > 0
  ? supplied
  : (process.env.QUARTO_PROJECT_OUTPUT_FILES ?? "").split(/\r?\n/).filter(Boolean);

for (const outputFile of outputFiles) {
  const absolute = path.resolve(process.cwd(), outputFile);
  if (path.extname(absolute).toLowerCase() !== ".pptx") continue;
  await patchPresentation(absolute);
}
