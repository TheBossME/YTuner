#!/usr/bin/env python3
"""
YTuner Station List Generator
A GUI application to create and manage station lists for YTuner in XML format.
"""

import tkinter as tk
from tkinter.scrolledtext import ScrolledText
from tkinter import ttk, messagebox, filedialog
import xml.etree.ElementTree as ET
import json
import configparser
import yaml
import re
import requests
import threading
from urllib.parse import urlparse
from typing import Dict, List, Optional
from datetime import datetime
from PIL import Image, ImageTk
import subprocess
import tempfile
import os
import uuid


class Station:
    def __init__(self, name: str = "", url: str = "", url_resolved: str = "", logo_url: str = "", description: str = "", 
                 country: str = "", language: str = "", tags: str = "", codec: str = "", bitrate: int = 0):
        self.name = name
        self.url = url
        self.url_resolved = url_resolved if url_resolved else url
        self.logo_url = logo_url
        self.description = description
        self.country = country
        self.language = language
        self.tags = tags
        self.codec = codec
        self.bitrate = bitrate
        self.uid = self._generate_uid()
    
    def _generate_uid(self) -> str:
        import hashlib
        content = f"{self.name}{self.url}"
        return f"MS_{hashlib.md5(content.encode()).hexdigest()[:12].upper()}"
    
    def to_dict(self) -> Dict:
        return {
            'name': self.name,
            'url': self.url,
            'url_resolved': self.url_resolved,
            'logo_url': self.logo_url,
            'description': self.description,
            'country': self.country,
            'language': self.language,
            'tags': self.tags,
            'codec': self.codec,
            'bitrate': self.bitrate,
            'uid': self.uid
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'Station':
        station = cls(data.get('name', ''), data.get('url', ''),
                     data.get('url_resolved', ''),
                     data.get('logo_url', ''), data.get('description', ''),
                     data.get('country', ''), data.get('language', ''),
                     data.get('tags', ''), data.get('codec', ''), data.get('bitrate', 0))
        if 'uid' in data:
            station.uid = data['uid']
        return station


class StationCategory:
    def __init__(self, name: str = ""):
        self.name = name
        self.stations: List[Station] = []
    
    def add_station(self, station: Station):
        self.stations.append(station)
    
    def remove_station(self, index: int):
        if 0 <= index < len(self.stations):
            del self.stations[index]
    
    def to_dict(self) -> Dict:
        return {
            'name': self.name,
            'stations': [station.to_dict() for station in self.stations]
        }
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'StationCategory':
        category = cls(data.get('name', ''))
        for station_data in data.get('stations', []):
            category.add_station(Station.from_dict(station_data))
        return category


class RadioBrowserAPI:
    def __init__(self):
        self.base_url = "https://de1.api.radio-browser.info"
        self.session = requests.Session()
        self.session.headers.update({'User-Agent': 'YTuner Station Generator/1.0'})
    
    def search_stations(self, name="", country="", language="", tag="", limit=100):
        """Search stations with filters"""
        url = f"{self.base_url}/json/stations/search"
        params = {
            'limit': limit,
            'hidebroken': 'true'
        }
        if name:
            params['name'] = name
        if country:
            params['country'] = country
        if language:
            params['language'] = language
        if tag:
            params['tag'] = tag
            
        try:
            response = self.session.get(url, params=params, timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            print(f"Error searching stations: {e}")
            return []
    
    def get_countries(self):
        """Get list of countries"""
        try:
            response = self.session.get(f"{self.base_url}/json/countries", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            print(f"Error getting countries: {e}")
            return []
    
    def get_languages(self):
        """Get list of languages"""
        try:
            response = self.session.get(f"{self.base_url}/json/languages", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            print(f"Error getting languages: {e}")
            return []
    
    def get_tags(self):
        """Get list of tags"""
        try:
            response = self.session.get(f"{self.base_url}/json/tags", timeout=10)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            print(f"Error getting tags: {e}")
            return []


class TuneInStreamTool:
    def __init__(self, parent_root, callback_add_station):
        self.root = tk.Toplevel(parent_root)
        self.callback_add_station = callback_add_station
        self.suchergebnisse = []
        self.current_logo_image = None
        self.search_thread = None
        self.cancel_search = False
        self.ffprobe_available = False
        self.setup_gui()
        self.check_ffprobe_availability()
        
    def setup_gui(self):
        self.root.title("🎵 TuneIn Radio Stream Manager")
        self.root.geometry("1000x700")
        self.root.configure(bg='#f0f0f0')
        
        # Style für ttk widgets
        style = ttk.Style()
        style.theme_use('clam')
        
        # Hauptframe
        main_frame = ttk.Frame(self.root, padding="10")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Suchbereich
        search_frame = ttk.LabelFrame(main_frame, text="🔍 Sender suchen", padding="10")
        search_frame.grid(row=0, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=(0, 10))
        
        ttk.Label(search_frame, text="Sendername:").grid(row=0, column=0, sticky=tk.W)
        self.eingabefeld = ttk.Entry(search_frame, font=("Segoe UI", 12), width=50)
        self.eingabefeld.grid(row=0, column=1, padx=(10, 0), sticky=(tk.W, tk.E))
        self.eingabefeld.bind('<Return>', lambda e: self.suche_und_zeige())
        
        self.such_button = ttk.Button(search_frame, text="🔎 Suchen", command=self.toggle_suche)
        self.such_button.grid(row=0, column=2, padx=(10, 0))
        
        search_frame.columnconfigure(1, weight=1)
        
        # Ergebnisbereich
        results_frame = ttk.LabelFrame(main_frame, text="📻 Suchergebnisse", padding="10")
        results_frame.grid(row=1, column=0, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0, 10))
        
        # Treeview für bessere Darstellung
        columns = ('Name', 'Codec', 'Beschreibung')
        self.tree = ttk.Treeview(results_frame, columns=columns, show='headings', height=12, selectmode='extended')
        
        # Spaltenbreiten definieren
        self.tree.heading('Name', text='🎵 Sendername')
        self.tree.heading('Codec', text='🔊 Codec')
        self.tree.heading('Beschreibung', text='📝 Beschreibung')
        
        self.tree.column('Name', width=250)
        self.tree.column('Codec', width=100)
        self.tree.column('Beschreibung', width=400)
        
        # Scrollbar für Treeview
        tree_scroll = ttk.Scrollbar(results_frame, orient="vertical", command=self.tree.yview)
        self.tree.configure(yscrollcommand=tree_scroll.set)
        
        self.tree.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        tree_scroll.grid(row=0, column=1, sticky=(tk.N, tk.S))
        
        self.tree.bind('<<TreeviewSelect>>', self.bei_auswahl)
        
        results_frame.columnconfigure(0, weight=1)
        results_frame.rowconfigure(0, weight=1)
        
        # Logo und Details Bereich
        info_frame = ttk.LabelFrame(main_frame, text="ℹ️ Sender-Details", padding="10")
        info_frame.grid(row=1, column=1, sticky=(tk.W, tk.E, tk.N, tk.S), padx=(10, 0), pady=(0, 10))
        
        # Logo
        self.logo_label = ttk.Label(info_frame, text="(kein Logo)", anchor="center")
        self.logo_label.grid(row=0, column=0, pady=(0, 10))
        
        # Details
        self.details_text = tk.Text(info_frame, height=8, width=30, wrap=tk.WORD, 
                                   font=("Segoe UI", 9), state=tk.DISABLED)
        self.details_text.grid(row=1, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Button zum Hinzufügen
        self.add_button = ttk.Button(info_frame, text="➕ In XML einfügen", 
                                   command=self.sende_auswahl, state=tk.DISABLED)
        self.add_button.grid(row=2, column=0, pady=(10, 0), sticky=(tk.W, tk.E))
        
        info_frame.rowconfigure(1, weight=1)
        
        # Ausgabebereich
        output_frame = ttk.LabelFrame(main_frame, text="📋 Ausgabe", padding="10")
        output_frame.grid(row=2, column=0, columnspan=2, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0, 10))
        
        self.ausgabe = ScrolledText(output_frame, wrap=tk.WORD, 
                                               font=("Consolas", 9), height=8)
        self.ausgabe.pack(fill=tk.BOTH, expand=True)
        
        # Status bar
        self.status_var = tk.StringVar(value="Bereit...")
        status_bar = ttk.Label(main_frame, textvariable=self.status_var, relief=tk.SUNKEN)
        status_bar.grid(row=3, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=(10, 0))
        
        # Grid weights
        main_frame.columnconfigure(0, weight=2)
        main_frame.columnconfigure(1, weight=1)
        main_frame.rowconfigure(1, weight=1)
        main_frame.rowconfigure(2, weight=1)
        
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)

    def suche_tunein(self, query):
        """Sucht nach Radiosendern über TuneIn API"""
        # Using a more generic TuneIn search endpoint that might be more stable
        # This is a guess without the specific API documentation.
        url = "https://opml.radiotime.com/Search.ashx"
        params = {"query": query, "render": "json"} # Request JSON if possible
        try:
            self.status_var.set(f"Suche nach '{query}'...")
            r = requests.get(url, params=params, timeout=10)
            r.raise_for_status()
            r.encoding = 'utf-8' # Explicitly set encoding

            if self.cancel_search: return []

            results = []
            try:
                # Try to parse as JSON first
                data = r.json()
                for item in data.get('body', []):
                    if item.get('type') == 'audio' and item.get('URL') and item.get('item') == 'station':
                        results.append({
                            "name": item.get("text", "Unbenannt"),
                            "url": item["URL"],
                            "logo": item.get("image", ""),
                            "description": item.get("subtext", ""),
                            "codec": "MP3", # Default, will be updated by ffprobe/ICY
                            "bitrate": item.get("bitrate", "128"),
                            "language": ""
                        })
            except json.JSONDecodeError:
                # Fallback to XML parsing if JSON fails
                try:
                    root = ET.fromstring(r.content)
                    for outline in root.findall(".//outline"):
                        if (outline.attrib.get("type") == "audio" and 
                            outline.attrib.get("URL") and 
                            outline.attrib.get("item") == "station"):
                            
                            results.append({
                                "name": outline.attrib.get("text", "Unbenannt"),
                                "url": outline.attrib["URL"],
                                "logo": outline.attrib.get("image", ""),
                                "description": outline.attrib.get("subtext", ""),
                                "codec": "MP3", # Default, will be updated by ffprobe/ICY
                                "bitrate": outline.attrib.get("bitrate", "128"),
                                "language": ""
                            })
                except Exception as xml_e:
                    self.status_var.set(f"Fehler beim Parsen der Antwort (XML/JSON): {str(xml_e)}")
                    self.ausgabe.insert(tk.END, f"❌ Parsing-Fehler: {str(xml_e)}\n")
                    return []
            
            self.status_var.set(f"{len(results)} Sender gefunden")
            return results
            
        except requests.exceptions.RequestException as req_e:
            self.status_var.set(f"Netzwerkfehler bei der Suche: {str(req_e)}")
            self.ausgabe.insert(tk.END, f"❌ Netzwerkfehler: {str(req_e)}\n")
            return []
        except Exception as e:
            self.status_var.set(f"Ein unerwarteter Fehler ist aufgetreten: {str(e)}")
            self.ausgabe.insert(tk.END, f"❌ Unerwarteter Fehler: {str(e)}\n")
            return []

    def extrahiere_stream_url(self, playlist_url):
        """Extrahiert die echte Stream-URL aus einer Playlist"""
        if not playlist_url:
            return None
            
        try:
            # Direkt prüfen ob es schon eine Stream-URL ist
            if playlist_url.startswith('http') and any(ext in playlist_url.lower() 
                                                     for ext in ['.mp3', '.aac', '.ogg']):
                return playlist_url
                
            r = requests.get(playlist_url, timeout=10, headers={
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
            })
            r.raise_for_status()
            
            content = r.text.strip()
            
            # M3U/PLS Format
            for line in content.splitlines():
                line = line.strip()
                if line.startswith('http'):
                    return line
                elif line.startswith('File1='):
                    return line.split('=', 1)[1]
                    
            return playlist_url
            
        except Exception as e:
            self.ausgabe.insert(tk.END, f"⚠️ Stream-URL Fehler: {str(e)}\n")
            return None

    def lese_icy_metadaten(self, stream_url):
        """Liest ICY-Metadaten mit ffprobe und direkten HTTP-Headers"""
        metadata = {"language": "", "codec": "MP3", "bitrate": "128"}
        
        # Methode 1: ffprobe (nur wenn verfügbar)
        if self.ffprobe_available:
            try:
                cmd = [
                    "ffprobe", "-v", "quiet", "-print_format", "json",
                    "-show_entries", "format_tags", "-timeout", "15000000",
                    stream_url
                ]
                result = subprocess.run(cmd, capture_output=True, text=True, timeout=15)
                
                if result.returncode == 0:
                    data = json.loads(result.stdout)
                    tags = data.get("format", {}).get("tags", {})
                    
                    # Verschiedene Tag-Varianten prüfen
                    for key, value in tags.items():
                        key_lower = key.lower()
                        if 'genre' in key_lower:
                            metadata["genre"] = value
                        elif 'language' in key_lower or 'lang' in key_lower:
                            metadata["language"] = value
                        elif 'codec' in key_lower:
                            metadata["codec"] = value
                        elif 'bitrate' in key_lower:
                            metadata["bitrate"] = str(value)
                            
            except Exception as e:
                self.ausgabe.insert(tk.END, f"⚠️ ffprobe Fehler: {str(e)}\n")
        
        # Methode 2: HTTP Headers direkt lesen
        try:
            headers = {
                'Icy-MetaData': '1',
                'User-Agent': 'VLC/3.0.0'
            }
            r = requests.get(stream_url, headers=headers, timeout=10, stream=True)
            
            # ICY Headers auslesen
            for key, value in r.headers.items():
                key_lower = key.lower()
                if 'icy-genre' in key_lower:
                    metadata["genre"] = value
                elif 'icy-br' in key_lower:
                    metadata["bitrate"] = value
                elif 'content-type' in key_lower:
                    if 'aac' in value.lower():
                        metadata["codec"] = "AAC"
                    elif 'ogg' in value.lower():
                        metadata["codec"] = "OGG"
                elif 'icy-language' in key_lower:
                    metadata["language"] = value
                        
            r.close()
            
        except Exception as e:
            self.ausgabe.insert(tk.END, f"⚠️ HTTP Headers Fehler: {str(e)}\n")
            
        return metadata

    def check_ffprobe_availability(self):
        try:
            subprocess.run(["ffprobe", "-version"], capture_output=True, check=True, timeout=5)
            self.ffprobe_available = True
            self.ausgabe.insert(tk.END, "✅ ffprobe gefunden. Erweiterte Metadaten werden geladen.\n")
        except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
            self.ffprobe_available = False
            self.ausgabe.insert(tk.END, "⚠️ ffprobe nicht gefunden oder nicht ausführbar. Metadaten können unvollständig sein.\n")
            messagebox.showwarning("ffprobe nicht gefunden", 
                                   "ffprobe wurde nicht gefunden oder ist nicht ausführbar. "
                                   "Einige Metadaten (z.B. detaillierte Codec-Informationen) "
                                   "können fehlen. Bitte stelle sicher, dass ffprobe installiert "
                                   "und im System-PATH verfügbar ist.")

    def toggle_suche(self):
        if self.search_thread and self.search_thread.is_alive():
            self.cancel_suche()
        else:
            self.such_button.config(state=tk.DISABLED)
            self.suche_und_zeige()

    def cancel_suche(self):
        if self.search_thread and self.search_thread.is_alive():
            self.cancel_search = True
            self.status_var.set("Suche wird abgebrochen...")

    def suche_und_zeige(self):
        """Startet die Suche in einem separaten Thread"""
        eingabe = self.eingabefeld.get().strip()
        if not eingabe:
            messagebox.showwarning("Eingabe fehlt", "Bitte gib einen Sendernamen ein.")
            return
            
        self.such_button.config(text="❌ Abbrechen")
        self.such_button.config(state=tk.NORMAL)
        self.tree.delete(*self.tree.get_children())
        self.ausgabe.delete(1.0, tk.END)
        self.logo_label.config(image="", text="(kein Logo)")
        self.add_button.config(state=tk.DISABLED)
        
        self.cancel_search = False
        self.search_thread = threading.Thread(target=self._suche_thread, args=(eingabe,))
        self.search_thread.daemon = True
        self.search_thread.start()

    def _suche_thread(self, eingabe):
        """Führt die Suche im Hintergrund aus"""
        self.suchergebnisse = self.suche_tunein(eingabe)
        
        if self.cancel_search:
            self.root.after(0, self._suche_abgebrochen)
            return

        self.root.after(0, self._update_suchergebnisse)

    def _suche_abgebrochen(self):
        """Setzt die GUI nach Abbruch zurück."""
        self.status_var.set("Suche abgebrochen.")
        self.such_button.config(text="🔎 Suchen")
        self.such_button.config(state=tk.NORMAL)

    def _update_suchergebnisse(self):
        """Aktualisiert die GUI mit den Suchergebnissen"""
        self.such_button.config(text="🔎 Suchen")
        self.such_button.config(state=tk.NORMAL)
        
        if self.cancel_search:
            return

        if not self.suchergebnisse:
            self.ausgabe.insert(tk.END, "❌ Keine Ergebnisse gefunden.\n")
            return
            
        for i, eintrag in enumerate(self.suchergebnisse):
            item_id = self.tree.insert('', 'end', values=(
                eintrag.get("name", ""),
                eintrag.get("codec", "Lade..."), # Initial placeholder
                eintrag.get("description", "")
            ))
            eintrag['tree_item_id'] = item_id # Store Treeview item ID
            
            # Start a thread to detect codec and update GUI
            threading.Thread(target=self._detect_codec_and_update_gui, args=(eintrag,)).start()
            
        self.ausgabe.insert(tk.END, f"✅ {len(self.suchergebnisse)} Sender gefunden\n")

    def _detect_codec_and_update_gui(self, eintrag):
        stream_url = self.extrahiere_stream_url(eintrag["url"])
        if stream_url:
            icy = self.lese_icy_metadaten(stream_url)
            detected_codec = icy.get("codec", "MP3")
            detected_language = icy.get("language", "")
            
            # Update the eintrag dictionary with detected values
            eintrag["codec"] = detected_codec
            eintrag["language"] = detected_language

            # Update the Treeview on the main thread
            self.root.after(0, self._update_treeview_codec, eintrag['tree_item_id'], detected_codec)

    def _update_treeview_codec(self, item_id, codec):
        # Get current values of the item
        current_values = list(self.tree.item(item_id, 'values'))
        # Update the codec (second column, index 1)
        current_values[1] = codec
        self.tree.item(item_id, values=current_values)

    def bei_auswahl(self, event):
        """Wird aufgerufen wenn ein Sender ausgewählt wird"""
        selection = self.tree.selection()
        if not selection:
            return
            
        item = self.tree.item(selection[0])
        index = self.tree.index(selection[0])
        
        if index < len(self.suchergebnisse):
            eintrag = self.suchergebnisse[index]
            self.zeige_details(eintrag)
            self.zeige_logo(eintrag)
            self.add_button.config(state=tk.NORMAL)

    def zeige_details(self, eintrag):
        """Zeigt Details des ausgewählten Senders"""
        self.details_text.config(state=tk.NORMAL)
        self.details_text.delete(1.0, tk.END)
        
        details = f"""📻 {eintrag.get('name', 'Unbekannt')}

🔊 Codec: {eintrag.get('codec', 'MP3')}
📡 Bitrate: {eintrag.get('bitrate', '128')} kbps
"""
        if eintrag.get('language') and eintrag.get('language') != "Unbekannt":
            details += f"🌐 Sprache: {eintrag.get('language')}\n"

        details += f"""📝 Beschreibung:
{eintrag.get('description', 'Keine Beschreibung verfügbar')}

🔗 URL:
{eintrag.get('url', '')[:100]}{'...' if len(eintrag.get('url', '')) > 100 else ''}"""
        
        self.details_text.insert(1.0, details)
        self.details_text.config(state=tk.DISABLED)

    def zeige_logo(self, eintrag):
        """Lädt und zeigt das Logo des Senders"""
        logo_url = eintrag.get("logo", "")
        if not logo_url:
            self.logo_label.config(image="", text="(kein Logo)")
            return
            
        def _load_logo():
            try:
                r = requests.get(logo_url, timeout=8, headers={
                    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
                })
                r.raise_for_status()
                
                with tempfile.NamedTemporaryFile(delete=False, suffix=".png") as tmp:
                    tmp.write(r.content)
                    tmp_path = tmp.name
                    
                img = Image.open(tmp_path)
                # Proportional auf max 120x120 skalieren
                img.thumbnail((120, 120), Image.Resampling.LANCZOS)
                
                img_tk = ImageTk.PhotoImage(img)
                
                # GUI Update im Hauptthread
                self.root.after(0, lambda: self._update_logo(img_tk, tmp_path))
                
            except Exception as e:
                self.root.after(0, lambda: self.logo_label.config(image="", text="(Logo nicht ladbar)"))
        
        # Logo in separatem Thread laden
        thread = threading.Thread(target=_load_logo)
        thread.daemon = True
        thread.start()

    def _update_logo(self, img_tk, tmp_path):
        """Aktualisiert das Logo im Hauptthread"""
        self.current_logo_image = img_tk  # Referenz behalten
        self.logo_label.config(image=img_tk, text="")
        try:
            os.unlink(tmp_path)
        except:
            pass

    def sende_auswahl(self):
        """Fügt die ausgewählten Sender zur XML-Datei hinzu"""
        selections = self.tree.selection()
        if not selections:
            messagebox.showwarning("Keine Auswahl", "Bitte mindestens einen Sender auswählen.")
            return
            
        added_count = 0
        for selected_item in selections:
            index = self.tree.index(selected_item)
            if index >= len(self.suchergebnisse):
                continue
                
            eintrag = self.suchergebnisse[index]
            
            # Stream-URL extrahieren
            self.status_var.set(f"Extrahiere Stream-URL für {eintrag['name']}...")
            stream_url = self.extrahiere_stream_url(eintrag["url"])
            if not stream_url:
                self.ausgabe.insert(tk.END, f"❌ Kein Stream gefunden für {eintrag['name']}\n")
                self.status_var.set(f"Fehler: Kein Stream gefunden für {eintrag['name']}")
                continue

            # ICY-Metadaten lesen
            self.status_var.set(f"Lese Metadaten für {eintrag['name']}...")
            icy = self.lese_icy_metadaten(stream_url)
            
            # Metadaten zusammenführen
            if icy["genre"] and not eintrag.get("genre"):
                eintrag["genre"] = icy["genre"]
            if icy["language"]:
                eintrag["language"] = icy["language"]
            if icy["codec"] != "MP3":
                eintrag["codec"] = icy["codec"]
            if icy["bitrate"] != "128":
                eintrag["bitrate"] = icy["bitrate"]

            # Pass data back to the main application via callback
            station_data = {
                'name': eintrag.get('name', ''),
                'url': stream_url,
                'url_resolved': stream_url, # TuneIn provides resolved URLs directly
                'logo_url': eintrag.get('logo', ''),
                'description': eintrag.get('description', ''),
                'country': eintrag.get('country', ''), # TuneIn doesn't always provide country directly
                'language': eintrag.get('language', ''),
                'tags': eintrag.get('genre', ''), # TuneIn uses 'genre' for tags
                'codec': eintrag.get('codec', ''),
                'bitrate': int(eintrag.get('bitrate', 0)) # Ensure bitrate is int
            }
            self.callback_add_station(station_data)
            added_count += 1
                
        if added_count > 0:
            self.status_var.set(f"{added_count} Sender erfolgreich hinzugefügt")
            self.ausgabe.insert(tk.END, f"✅ {added_count} station(s) added to main application.\n")
        else:
            self.status_var.set("Keine Sender hinzugefügt")

    def _format_xml_tree(self, elem, level=0):
        """Formatiert XML für bessere Lesbarkeit"""
        i = "\n" + level * "  "
        if len(elem):
            if not elem.text or not elem.text.strip():
                elem.text = i + "  "
            if not elem.tail or not elem.tail.strip():
                elem.tail = i
            for subelem in elem:
                self._format_xml_tree(subelem, level + 1)
            if not subelem.tail or not subelem.tail.strip():
                subelem.tail = i
        else:
            if level and (not elem.tail or not elem.tail.strip()):
                elem.tail = i


class YTunerStationGenerator:

    def __init__(self):
        self.root = tk.Tk()
        self.root.title("YTuner Station List Generator")
        self.root.geometry("1200x800")
        
        self.categories: List[StationCategory] = []
        self.current_category: Optional[StationCategory] = None
        self.api = RadioBrowserAPI()
        self.search_results = []
        
        self.setup_ui()
        self.setup_menu()
    
    def setup_menu(self):
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)
        
        file_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="File", menu=file_menu)
        file_menu.add_command(label="New", command=self.new_file)
        file_menu.add_separator()
        file_menu.add_command(label="Import XML and Merge", command=self.import_xml_and_merge)
        file_menu.add_command(label="Import INI", command=self.import_ini)
        file_menu.add_command(label="Import YAML", command=self.import_yaml)
        file_menu.add_separator()
        file_menu.add_command(label="Export XML", command=self.export_xml)
        file_menu.add_command(label="Export INI", command=self.export_ini)
        file_menu.add_command(label="Export YAML", command=self.export_yaml)
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self.root.quit)
        
        radio_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Search Stations", menu=radio_menu)
        radio_menu.add_command(label="Search Radiobrowser", command=self.open_search_window)
        radio_menu.add_command(label="Search TuneIn", command=self.open_tunein_search_window)
    
    def setup_ui(self):
        # Main container
        main_frame = ttk.Frame(self.root, padding="10")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Configure grid weights
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)
        main_frame.columnconfigure(1, weight=1)
        main_frame.rowconfigure(1, weight=1)
        
        # Categories section
        categories_frame = ttk.LabelFrame(main_frame, text="Categories", padding="5")
        categories_frame.grid(row=0, column=0, rowspan=2, sticky=(tk.W, tk.E, tk.N, tk.S), padx=(0, 10))
        categories_frame.columnconfigure(0, weight=1)
        categories_frame.rowconfigure(1, weight=1)
        
        # Category controls
        cat_controls = ttk.Frame(categories_frame)
        cat_controls.grid(row=0, column=0, sticky=(tk.W, tk.E), pady=(0, 5))
        cat_controls.columnconfigure(0, weight=1)
        
        self.category_entry = ttk.Entry(cat_controls)
        self.category_entry.grid(row=0, column=0, sticky=(tk.W, tk.E), padx=(0, 5))
        
        ttk.Button(cat_controls, text="Add Category", command=self.add_category).grid(row=0, column=1)
        
        # Category list
        self.category_listbox = tk.Listbox(categories_frame, width=25)
        self.category_listbox.grid(row=1, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        self.category_listbox.bind('<<ListboxSelect>>', self.on_category_select)
        
        cat_buttons = ttk.Frame(categories_frame)
        cat_buttons.grid(row=2, column=0, sticky=(tk.W, tk.E), pady=(5, 0))
        
        ttk.Button(cat_buttons, text="Remove Category", command=self.remove_category).pack(side=tk.LEFT, padx=(0, 5))
        
        # Stations section
        stations_frame = ttk.LabelFrame(main_frame, text="Stations", padding="5")
        stations_frame.grid(row=0, column=1, rowspan=2, sticky=(tk.W, tk.E, tk.N, tk.S))
        stations_frame.columnconfigure(0, weight=1)
        stations_frame.rowconfigure(1, weight=1)
        
        # Station form
        form_frame = ttk.Frame(stations_frame)
        form_frame.grid(row=0, column=0, sticky=(tk.W, tk.E), pady=(0, 10))
        form_frame.columnconfigure(1, weight=1)
        
        ttk.Label(form_frame, text="Station Name:").grid(row=0, column=0, sticky=tk.W, pady=2)
        self.station_name_entry = ttk.Entry(form_frame)
        self.station_name_entry.grid(row=0, column=1, sticky=(tk.W, tk.E), padx=(5, 0), pady=2)
        
        ttk.Label(form_frame, text="Stream URL:").grid(row=1, column=0, sticky=tk.W, pady=2)
        self.station_url_entry = ttk.Entry(form_frame)
        self.station_url_entry.grid(row=1, column=1, sticky=(tk.W, tk.E), padx=(5, 0), pady=2)
        
        ttk.Label(form_frame, text="Logo URL:").grid(row=2, column=0, sticky=tk.W, pady=2)
        self.station_logo_entry = ttk.Entry(form_frame)
        self.station_logo_entry.grid(row=2, column=1, sticky=(tk.W, tk.E), padx=(5, 0), pady=2)
        
        ttk.Label(form_frame, text="Description:").grid(row=3, column=0, sticky=tk.W, pady=2)
        self.station_desc_entry = ttk.Entry(form_frame)
        self.station_desc_entry.grid(row=3, column=1, sticky=(tk.W, tk.E), padx=(5, 0), pady=2)
        
        # Station buttons
        button_frame = ttk.Frame(form_frame)
        button_frame.grid(row=4, column=0, columnspan=2, pady=10)
        
        ttk.Button(button_frame, text="Add Station", command=self.add_station).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(button_frame, text="Update Station", command=self.update_station).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(button_frame, text="Clear Form", command=self.clear_station_form).pack(side=tk.LEFT)
        
        # Stations list
        list_frame = ttk.Frame(stations_frame)
        list_frame.grid(row=1, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        list_frame.columnconfigure(0, weight=1)
        list_frame.rowconfigure(0, weight=1)
        
        # Treeview for stations
        columns = ('name', 'url', 'logo')
        self.stations_tree = ttk.Treeview(list_frame, columns=columns, show='headings', height=15)
        self.stations_tree.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Configure columns
        self.stations_tree.heading('name', text='Station Name')
        self.stations_tree.heading('url', text='Stream URL')
        self.stations_tree.heading('logo', text='Logo URL')
        
        self.stations_tree.column('name', width=200)
        self.stations_tree.column('url', width=300)
        self.stations_tree.column('logo', width=200)
        
        # Scrollbar for treeview
        scrollbar = ttk.Scrollbar(list_frame, orient=tk.VERTICAL, command=self.stations_tree.yview)
        scrollbar.grid(row=0, column=1, sticky=(tk.N, tk.S))
        self.stations_tree.configure(yscrollcommand=scrollbar.set)
        
        # Bind events
        self.stations_tree.bind('<<TreeviewSelect>>', self.on_station_select)
        self.stations_tree.bind('<Double-1>', self.edit_station)
        
        # Station management buttons
        station_buttons = ttk.Frame(stations_frame)
        station_buttons.grid(row=2, column=0, sticky=(tk.W, tk.E), pady=(5, 0))
        
        ttk.Button(station_buttons, text="Remove Station", command=self.remove_station).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(station_buttons, text="Validate URLs", command=self.validate_urls).pack(side=tk.LEFT)
    
    def add_category(self):
        name = self.category_entry.get().strip()
        if not name:
            messagebox.showwarning("Warning", "Please enter a category name.")
            return
        
        if any(cat.name == name for cat in self.categories):
            messagebox.showwarning("Warning", "Category already exists.")
            return
        
        category = StationCategory(name)
        self.categories.append(category)
        self.refresh_categories()
        self.category_entry.delete(0, tk.END)
    
    def remove_category(self):
        selection = self.category_listbox.curselection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a category to remove.")
            return
        
        index = selection[0]
        category = self.categories[index]
        
        if messagebox.askyesno("Confirm", f"Remove category '{category.name}' and all its stations?"):
            del self.categories[index]
            self.current_category = None
            self.refresh_categories()
            self.refresh_stations()
    
    def on_category_select(self, event):
        selection = self.category_listbox.curselection()
        if selection:
            index = selection[0]
            self.current_category = self.categories[index]
            self.refresh_stations()
    
    def add_station(self):
        if not self.current_category:
            messagebox.showwarning("Warning", "Please select a category first.")
            return
        
        name = self.station_name_entry.get().strip()
        url = self.station_url_entry.get().strip()
        
        if not name or not url:
            messagebox.showwarning("Warning", "Please enter station name and URL.")
            return
        
        if not self.is_valid_url(url):
            messagebox.showwarning("Warning", "Please enter a valid stream URL.")
            return
        
        logo_url = self.station_logo_entry.get().strip()
        if logo_url and not self.is_valid_url(logo_url):
            messagebox.showwarning("Warning", "Please enter a valid logo URL.")
            return
        
        description = self.station_desc_entry.get().strip()
        
        station = Station(name, url, "", logo_url, description)
        self.current_category.add_station(station)
        self.refresh_stations()
        self.clear_station_form()
    
    def update_station(self):
        selection = self.stations_tree.selection()
        if not selection or not self.current_category:
            messagebox.showwarning("Warning", "Please select a station to update.")
            return
        
        item = selection[0]
        index = self.stations_tree.index(item)
        
        name = self.station_name_entry.get().strip()
        url = self.station_url_entry.get().strip()
        
        if not name or not url:
            messagebox.showwarning("Warning", "Please enter station name and URL.")
            return
        
        if not self.is_valid_url(url):
            messagebox.showwarning("Warning", "Please enter a valid stream URL.")
            return
        
        logo_url = self.station_logo_entry.get().strip()
        if logo_url and not self.is_valid_url(logo_url):
            messagebox.showwarning("Warning", "Please enter a valid logo URL.")
            return
        
        description = self.station_desc_entry.get().strip()
        
        station = self.current_category.stations[index]
        station.name = name
        station.url = url
        station.logo_url = logo_url
        station.description = description
        station.uid = station._generate_uid()
        
        self.refresh_stations()
        self.clear_station_form()
    
    def remove_station(self):
        selection = self.stations_tree.selection()
        if not selection or not self.current_category:
            messagebox.showwarning("Warning", "Please select a station to remove.")
            return
        
        item = selection[0]
        index = self.stations_tree.index(item)
        station = self.current_category.stations[index]
        
        if messagebox.askyesno("Confirm", f"Remove station '{station.name}'?"):
            self.current_category.remove_station(index)
            self.refresh_stations()
            self.clear_station_form()
    
    def on_station_select(self, event):
        selection = self.stations_tree.selection()
        if selection and self.current_category:
            item = selection[0]
            index = self.stations_tree.index(item)
            station = self.current_category.stations[index]
            
            self.station_name_entry.delete(0, tk.END)
            self.station_name_entry.insert(0, station.name)
            
            self.station_url_entry.delete(0, tk.END)
            self.station_url_entry.insert(0, station.url)
            
            self.station_logo_entry.delete(0, tk.END)
            self.station_logo_entry.insert(0, station.logo_url)
            
            self.station_desc_entry.delete(0, tk.END)
            self.station_desc_entry.insert(0, station.description)
    
    def edit_station(self, event):
        self.on_station_select(event)
    
    def clear_station_form(self):
        self.station_name_entry.delete(0, tk.END)
        self.station_url_entry.delete(0, tk.END)
        self.station_logo_entry.delete(0, tk.END)
        self.station_desc_entry.delete(0, tk.END)
    
    def refresh_categories(self):
        self.category_listbox.delete(0, tk.END)
        for category in self.categories:
            self.category_listbox.insert(tk.END, category.name)
    
    def refresh_stations(self):
        for item in self.stations_tree.get_children():
            self.stations_tree.delete(item)
        
        if self.current_category:
            for station in self.current_category.stations:
                self.stations_tree.insert('', tk.END, values=(
                    station.name,
                    station.url,
                    station.logo_url
                ))
    
    def is_valid_url(self, url: str) -> bool:
        try:
            result = urlparse(url)
            return all([result.scheme, result.netloc])
        except:
            return False
    
    def validate_urls(self):
        if not self.current_category:
            messagebox.showwarning("Warning", "Please select a category first.")
            return
        
        invalid_stations = []
        for i, station in enumerate(self.current_category.stations):
            if not self.is_valid_url(station.url):
                invalid_stations.append(f"Station {i+1}: {station.name} - Invalid URL: {station.url}")
            
            if station.logo_url and not self.is_valid_url(station.logo_url):
                invalid_stations.append(f"Station {i+1}: {station.name} - Invalid Logo URL: {station.logo_url}")
        
        if invalid_stations:
            messagebox.showwarning("URL Validation", "Invalid URLs found:\n\n" + "\n".join(invalid_stations))
        else:
            messagebox.showinfo("URL Validation", "All URLs are valid!")
    
    def new_file(self):
        if messagebox.askyesno("Confirm", "Create new file? All current data will be lost."):
            self.categories.clear()
            self.current_category = None
            self.refresh_categories()
            self.refresh_stations()
            self.clear_station_form()
    
    def import_ini(self):
        filename = filedialog.askopenfilename(
            title="Import INI File",
            filetypes=[("INI files", "*.ini"), ("All files", "*.* Experiencing")]
        )
        
        if filename:
            try:
                config = configparser.ConfigParser()
                config.read(filename)
                
                self.categories.clear()
                
                for section_name in config.sections():
                    category = StationCategory(section_name)
                    
                    for key, value in config.items(section_name):
                        parts = value.split('|')
                        url = parts[0].strip()
                        logo_url = parts[1].strip() if len(parts) > 1 else ""
                        
                        station = Station(key, url, "", logo_url)
                        category.add_station(station)
                    
                    self.categories.append(category)
                
                self.current_category = None
                self.refresh_categories()
                self.refresh_stations()
                messagebox.showinfo("Success", f"Imported {len(self.categories)} categories from INI file.")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to import INI file: {str(e)}")
    
    def import_yaml(self):
        filename = filedialog.askopenfilename(
            title="Import YAML File",
            filetypes=[("YAML files", "*.yaml *.yml"), ("All files", "*.* Experiencing")]
        )
        
        if filename:
            try:
                with open(filename, 'r', encoding='utf-8') as f:
                    data = yaml.safe_load(f)
                
                self.categories.clear()
                
                for category_name, stations_data in data.items():
                    category = StationCategory(category_name)
                    
                    for station_line in stations_data:
                        if isinstance(station_line, str):
                            parts = station_line.split(': ', 1)
                            if len(parts) == 2:
                                name = parts[0].strip()
                                url_parts = parts[1].split('|')
                                url = url_parts[0].strip()
                                logo_url = url_parts[1].strip() if len(url_parts) > 1 else ""
                                
                                station = Station(name, url, "", logo_url)
                                category.add_station(station)
                    
                    self.categories.append(category)
                
                self.current_category = None
                self.refresh_categories()
                self.refresh_stations()
                messagebox.showinfo("Success", f"Imported {len(self.categories)} categories from YAML file.")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to import YAML file: {str(e)}")
    
    def export_xml(self):
        if not self.categories:
            messagebox.showwarning("Warning", "No data to export.")
            return

        filename = filedialog.asksaveasfilename(
            title="Export XML File",
            defaultextension=".xml",
            filetypes=[("XML files", "*.xml"), ("All files", "*.* Experiencing")]
        )

        if filename:
            try:
                root = ET.Element("MyStations")
                
                version = ET.SubElement(root, "Version")
                version.text = "1.1"
                
                list_of_items = ET.SubElement(root, "ListOfItems")
                
                item_index = 0
                for category in self.categories:
                    for station in category.stations:
                        item = ET.SubElement(list_of_items, "Item")
                        item.set("id", station.uid)
                        item.set("idx", str(item_index))
                        
                        ET.SubElement(item, "Name").text = station.name
                        ET.SubElement(item, "URL").text = station.url
                        ET.SubElement(item, "URLResolved").text = station.url_resolved
                        ET.SubElement(item, "Description").text = station.description
                        ET.SubElement(item, "Logo").text = station.logo_url
                        ET.SubElement(item, "Genres").text = station.tags if station.tags else category.name
                        ET.SubElement(item, "Languages").text = station.language
                        ET.SubElement(item, "Location").text = station.country
                        
                        mime_type = f"audio/{station.codec.lower()}" if station.codec else "audio/mpeg"
                        ET.SubElement(item, "Mime").text = mime_type
                        
                        ET.SubElement(item, "Codec").text = station.codec if station.codec else "MP3"
                        ET.SubElement(item, "Bitrate").text = str(station.bitrate) if station.bitrate > 0 else "128"
                        
                        now_time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
                        ET.SubElement(item, "LastCheckedOK").text = "1"
                        ET.SubElement(item, "LastCheckedOKTime").text = now_time
                        ET.SubElement(item, "LastCheckedTime").text = now_time
                        
                        item_index += 1

                def indent(elem, level=0):
                    i = "\n" + level*"  "
                    if len(elem):
                        if not elem.text or not elem.text.strip():
                            elem.text = i + "  "
                        if not elem.tail or not elem.tail.strip():
                            elem.tail = i
                        for elem in elem:
                            indent(elem, level+1)
                        if not elem.tail or not elem.tail.strip():
                            elem.tail = i
                    else:
                        if level and (not elem.tail or not elem.tail.strip()):
                            elem.tail = i

                indent(root)
                
                tree = ET.ElementTree(root)
                tree.write(filename, encoding='utf-8', xml_declaration=True)
                
                total_items = sum(len(cat.stations) for cat in self.categories)
                messagebox.showinfo("Success", f"Exported {total_items} stations to XML file.")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to export XML file: {str(e)}")
    
    def export_ini(self):
        if not self.categories:
            messagebox.showwarning("Warning", "No data to export.")
            return
        
        filename = filedialog.asksaveasfilename(
            title="Export INI File",
            defaultextension=".ini",
            filetypes=[("INI files", "*.ini"), ("All files", "*.* Experiencing")]
        )
        
        if filename:
            try:
                config = configparser.ConfigParser()
                
                for category in self.categories:
                    config.add_section(category.name)
                    for station in category.stations:
                        value = station.url
                        if station.logo_url:
                            value += f"|{station.logo_url}"
                        config.set(category.name, station.name, value)
                
                with open(filename, 'w', encoding='utf-8') as f:
                    config.write(f)
                
                total_stations = sum(len(cat.stations) for cat in self.categories)
                messagebox.showinfo("Success", f"Exported {total_stations} stations to INI file.")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to export INI file: {str(e)}")
    
    def export_yaml(self):
        if not self.categories:
            messagebox.showwarning("Warning", "No data to export.")
            return
        
        filename = filedialog.asksaveasfilename(
            title="Export YAML File",
            defaultextension=".yaml",
            filetypes=[("YAML files", "*.yaml *.yml"), ("All files", "*.* Experiencing")]
        )
        
        if filename:
            try:
                data = {}
                
                for category in self.categories:
                    stations_list = []
                    for station in category.stations:
                        station_line = f"{station.name}: {station.url}"
                        if station.logo_url:
                            station_line += f"|{station.logo_url}"
                        stations_list.append(station_line)
                    data[category.name] = stations_list
                
                with open(filename, 'w', encoding='utf-8') as f:
                    yaml.dump(data, f, default_flow_style=False, allow_unicode=True)
                
                total_stations = sum(len(cat.stations) for cat in self.categories)
                messagebox.showinfo("Success", f"Exported {total_stations} stations to YAML file.")
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to export YAML file: {str(e)}")
    
    def import_xml_and_merge(self):
        filename = filedialog.askopenfilename(
            title="Import XML File to Merge",
            filetypes=[("XML files", "*.xml"), ("All files", "*.*")]
        )

        if filename:
            try:
                tree = ET.parse(filename)
                root = tree.getroot()
                
                merged_count = 0
                for item_elem in root.findall(".//Item"):
                    # Robustly extract text content, handling None for missing elements or empty text
                    def get_text_or_empty(element, tag):
                        found_element = element.find(tag)
                        return found_element.text if found_element is not None and found_element.text is not None else ""

                    name = get_text_or_empty(item_elem, "Name")
                    url = get_text_or_empty(item_elem, "URL")
                    url_resolved = get_text_or_empty(item_elem, "URLResolved")
                    logo_url = get_text_or_empty(item_elem, "Logo")
                    description = get_text_or_empty(item_elem, "Description")
                    country = get_text_or_empty(item_elem, "Location")
                    language = get_text_or_empty(item_elem, "Languages")
                    tags = get_text_or_empty(item_elem, "Genres")
                    codec = get_text_or_empty(item_elem, "Codec")
                    
                    bitrate_str = get_text_or_empty(item_elem, "Bitrate")
                    bitrate = int(bitrate_str) if bitrate_str.isdigit() else 0

                    # Basic validation for essential fields
                    if not name or not url:
                        print(f"Skipping station due to missing Name or URL: Name='{name}', URL='{url}'")
                        continue
                    
                    if not self.is_valid_url(url):
                        print(f"Skipping station '{name}' due to invalid URL: {url}")
                        continue

                    # Check for duplicates based on name and URL
                    is_duplicate = False
                    for category in self.categories:
                        for existing_station in category.stations:
                            if existing_station.name == name and existing_station.url == url:
                                is_duplicate = True
                                break
                        if is_duplicate:
                            break
                    
                    if not is_duplicate:
                        # Add to a default category or create one if none exists
                        if not self.categories:
                            self.add_category_internal("Imported Stations") # Internal method to add category without GUI interaction
                        
                        # Find a suitable category (e.g., by genre or create a new one)
                        target_category = None
                        if tags:
                            for cat in self.categories:
                                if cat.name.lower() == tags.lower():
                                    target_category = cat
                                    break
                        if not target_category:
                            # If no matching category, add to the first existing category or "Imported Stations"
                            if self.categories:
                                target_category = self.categories[0]
                            else:
                                # This case should ideally not be reached if add_category_internal is called
                                pass 

                        if target_category:
                            station = Station(name, url, url_resolved, logo_url, description, country, language, tags, codec, bitrate)
                            target_category.add_station(station)
                            merged_count += 1
                
                self.refresh_categories()
                self.refresh_stations()
                messagebox.showinfo("Success", f"Merged {merged_count} new station(s) from XML file.")
                
                if merged_count > 0:
                    if messagebox.askyesno("Save Merged File", "Do you want to save the merged station list to a new XML file?"):
                        self.export_xml() # Re-use existing export function
                
            except Exception as e:
                messagebox.showerror("Error", f"Failed to import and merge XML file: {str(e)}")

    def add_category_internal(self, name):
        # Helper method to add a category without triggering GUI events or warnings
        if not any(cat.name == name for cat in self.categories):
            category = StationCategory(name)
            self.categories.append(category)
            return True
        return False

    def open_search_window(self):
        search_window = tk.Toplevel(self.root)
        search_window.title("Radio Browser Search")
        search_window.geometry("1000x600")
        search_window.transient(self.root)
        search_window.grab_set()
        
        # Search frame
        search_frame = ttk.LabelFrame(search_window, text="Search Filters", padding="10")
        search_frame.pack(fill=tk.X, padx=10, pady=5)
        
        # Filter controls
        filter_frame = ttk.Frame(search_frame)
        filter_frame.pack(fill=tk.X)
        filter_frame.columnconfigure(1, weight=1)
        filter_frame.columnconfigure(3, weight=1)
        filter_frame.columnconfigure(5, weight=1)
        
        ttk.Label(filter_frame, text="Name:").grid(row=0, column=0, sticky=tk.W, padx=(0, 5))
        self.search_name_var = tk.StringVar()
        ttk.Entry(filter_frame, textvariable=self.search_name_var).grid(row=0, column=1, sticky=(tk.W, tk.E), padx=(0, 10))
        
        ttk.Label(filter_frame, text="Country:").grid(row=0, column=2, sticky=tk.W, padx=(0, 5))
        self.search_country_var = tk.StringVar()
        country_combo = ttk.Combobox(filter_frame, textvariable=self.search_country_var, state="readonly")
        country_combo.grid(row=0, column=3, sticky=(tk.W, tk.E), padx=(0, 10))
        
        ttk.Label(filter_frame, text="Language:").grid(row=0, column=4, sticky=tk.W, padx=(0, 5))
        self.search_language_var = tk.StringVar()
        language_combo = ttk.Combobox(filter_frame, textvariable=self.search_language_var, state="readonly")
        language_combo.grid(row=0, column=5, sticky=(tk.W, tk.E))
        
        ttk.Label(filter_frame, text="Tag:").grid(row=1, column=0, sticky=tk.W, padx=(0, 5), pady=(5, 0))
        self.search_tag_var = tk.StringVar()
        tag_combo = ttk.Combobox(filter_frame, textvariable=self.search_tag_var, state="readonly")
        tag_combo.grid(row=1, column=1, sticky=(tk.W, tk.E), padx=(0, 10), pady=(5, 0))
        
        ttk.Button(filter_frame, text="Search", command=self.search_radio_browser).grid(row=1, column=2, padx=(0, 10), pady=(5, 0))
        ttk.Button(filter_frame, text="Clear", command=self.clear_search_filters).grid(row=1, column=3, pady=(5, 0))
        
        # Results frame
        results_frame = ttk.LabelFrame(search_window, text="Search Results", padding="10")
        results_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)
        
        # Results treeview
        columns = ('name', 'country', 'language', 'tags', 'codec', 'bitrate')
        self.search_tree = ttk.Treeview(results_frame, columns=columns, show='headings', height=15, selectmode='extended')
        self.search_tree.pack(fill=tk.BOTH, expand=True, side=tk.LEFT)
        
        # Configure columns
        self.search_tree.heading('name', text='Station Name')
        self.search_tree.heading('country', text='Country')
        self.search_tree.heading('language', text='Language')
        self.search_tree.heading('tags', text='Tags')
        self.search_tree.heading('codec', text='Codec')
        self.search_tree.heading('bitrate', text='Bitrate')
        
        self.search_tree.column('name', width=200)
        self.search_tree.column('country', width=100)
        self.search_tree.column('language', width=100)
        self.search_tree.column('tags', width=150)
        self.search_tree.column('codec', width=80)
        self.search_tree.column('bitrate', width=80)
        
        # Scrollbar for search results
        search_scrollbar = ttk.Scrollbar(results_frame, orient=tk.VERTICAL, command=self.search_tree.yview)
        search_scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.search_tree.configure(yscrollcommand=search_scrollbar.set)
        self.search_tree.bind('<Control-a>', self.select_all_search_results)
        self.search_tree.bind('<Control-A>', self.select_all_search_results)
        
        # Buttons frame
        buttons_frame = ttk.Frame(search_window)
        buttons_frame.pack(fill=tk.X, padx=10, pady=5)
        
        ttk.Button(buttons_frame, text="Add Selected to Category", command=self.add_selected_stations).pack(side=tk.LEFT, padx=(0, 5))
        ttk.Button(buttons_frame, text="Close", command=search_window.destroy).pack(side=tk.RIGHT)
        
        # Load dropdown data
        self.load_search_dropdowns(country_combo, language_combo, tag_combo)
    
    def open_tunein_search_window(self):
        # Pass the main application's root and a callback function
        TuneInStreamTool(self.root, self.add_station_from_tunein)

    def add_station_from_tunein(self, station_data):
        if not self.current_category:
            messagebox.showwarning("Warning", "Please select a category first in the main window to add TuneIn stations.")
            return
        
        # Create a Station object from the received data
        station = Station(
            name=station_data.get('name', ''),
            url=station_data.get('url', ''),
            url_resolved=station_data.get('url_resolved', ''),
            logo_url=station_data.get('logo_url', ''),
            description=station_data.get('description', ''),
            country=station_data.get('country', ''),
            language=station_data.get('language', ''),
            tags=station_data.get('tags', ''),
            codec=station_data.get('codec', ''),
            bitrate=station_data.get('bitrate', 0)
        )
        
        # Check for duplicates before adding
        is_duplicate = False
        for existing_station in self.current_category.stations:
            if existing_station.name == station.name and existing_station.url == station.url:
                is_duplicate = True
                break
        
        if not is_duplicate:
            self.current_category.add_station(station)
            self.refresh_stations()
            messagebox.showinfo("Success", f"Added '{station.name}' from TuneIn to category '{self.current_category.name}'.")
        else:
            messagebox.showinfo("Info", f"Station '{station.name}' from TuneIn already exists in category '{self.current_category.name}'.")

    def load_search_dropdowns(self, country_combo, language_combo, tag_combo):
        def load_data():
            try:
                countries = self.api.get_countries()
                # Sort countries alphabetically by name
                country_names = [''] + sorted([c.get('name', '') for c in countries if c.get('name')], key=lambda x: x.lower())
                country_combo['values'] = country_names
                
                languages = self.api.get_languages()
                # Sort languages alphabetically by name
                language_names = [''] + sorted([l.get('name', '') for l in languages if l.get('name')], key=lambda x: x.lower())
                language_combo['values'] = language_names
                
                tags = self.api.get_tags()
                # Sort tags alphabetically by name and limit to 100 most popular tags
                tag_names = [''] + sorted([t.get('name', '') for t in tags if t.get('name')], key=lambda x: x.lower())[:100]
                tag_combo['values'] = tag_names
            except Exception as e:
                print(f"Error loading dropdown data: {e}")
        
        threading.Thread(target=load_data, daemon=True).start()
    
    def search_radio_browser(self):
        def perform_search():
            try:
                name = self.search_name_var.get()
                country = self.search_country_var.get()
                language = self.search_language_var.get()
                tag = self.search_tag_var.get()
                
                self.search_results = self.api.search_stations(name=name, country=country, language=language, tag=tag)
                
                # Update UI in main thread
                self.root.after(0, self.update_search_results)
            except Exception as e:
                print(f"Error searching: {e}")
        
        threading.Thread(target=perform_search, daemon=True).start()
    
    def update_search_results(self):
        # Clear existing results
        for item in self.search_tree.get_children():
            self.search_tree.delete(item)
        
        # Add new results
        for station_data in self.search_results:
            tags = ', '.join(station_data.get('tags', '').split(',')[:3])  # Show first 3 tags
            self.search_tree.insert('', tk.END, values=(
                station_data.get('name', ''),
                station_data.get('country', ''),
                station_data.get('language', ''),
                tags,
                station_data.get('codec', ''),
                station_data.get('bitrate', 0)
            ))
    
    def clear_search_filters(self):
        self.search_name_var.set('')
        self.search_country_var.set('')
        self.search_language_var.set('')
        self.search_tag_var.set('')
    
    def select_all_search_results(self, event=None):
        for item in self.search_tree.get_children():
            self.search_tree.selection_add(item)

    def add_selected_stations(self):
        if not self.current_category:
            messagebox.showwarning("Warning", "Please select a category first.")
            return
        
        selection = self.search_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select one or more stations to add.")
            return
        
        added_count = 0
        for item in selection:
            index = self.search_tree.index(item)
            if index < len(self.search_results):
                station_data = self.search_results[index]
                
                station = Station(
                    name=station_data.get('name', ''),
                    url=station_data.get('url', ''),
                    url_resolved=station_data.get('url_resolved', ''),
                    logo_url=station_data.get('favicon', ''),
                    description=station_data.get('homepage', ''),
                    country=station_data.get('country', ''),
                    language=station_data.get('language', ''),
                    tags=station_data.get('tags', ''),
                    codec=station_data.get('codec', ''),
                    bitrate=station_data.get('bitrate', 0)
                )
                
                self.current_category.add_station(station)
                added_count += 1
        
        if added_count > 0:
            self.refresh_stations()
            messagebox.showinfo("Success", f"Added {added_count} station(s) to category '{self.current_category.name}'.")
    
    def run(self):
        self.root.mainloop()


if __name__ == "__main__":
    app = YTunerStationGenerator()
    app.run()
